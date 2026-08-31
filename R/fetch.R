dir.create('content/post', showWarnings = FALSE)
d = Sys.Date()

if (!require('xfun')) install.packages('xfun')
xfun::pkg_load2('jsonlite', 'curl')

xfun:::pkg_update()

# authenticate against the PDS; the public AppView (api.bsky.app) blocks
# datacenter IPs (e.g. GitHub Actions) with HTTP 403, so we go through an
# authenticated session which is not IP-blocked
bsky_login = function() {
  id = Sys.getenv('BSKY_HANDLE'); pw = Sys.getenv('BSKY_APP_PASSWORD')
  if (id == '' || pw == '')
    stop('Set BSKY_HANDLE and BSKY_APP_PASSWORD (an app password, not your main password).')
  h = curl::new_handle()
  curl::handle_setheaders(h, 'Content-Type' = 'application/json')
  curl::handle_setopt(
    h, post = TRUE,
    postfields = jsonlite::toJSON(
      list(identifier = id, password = pw), auto_unbox = TRUE
    )
  )
  res = curl::curl_fetch_memory(
    'https://bsky.social/xrpc/com.atproto.server.createSession', handle = h
  )
  if (res$status_code != 200)
    stop('Bluesky login failed (HTTP ', res$status_code, '): ', rawToChar(res$content))
  jsonlite::fromJSON(rawToChar(res$content))$accessJwt
}

jwt = bsky_login()

if (!file.exists(f <- 'R/keywords.csv')) writeLines('query,since,limit', f)
m = read.csv(f, colClasses = c('character', 'character', 'integer'))
d = as.character(d)
x = NULL; t = paste('Posts retrieved on', d); n_like = 0  # markdown text, post title
ids = NULL  # ids of posts that have already been included

search_bsky = function(query, since = 0, limit = 0) {
  extra = ''; fmt = '%Y-%m-%dT%H:%M:%OSZ'
  since = if (since <= 0) as.Date(0) else strptime(since, fmt, tz = 'UTC') + 1  # offset by 1 sec
  since = format(since, fmt)
  if (limit <= 0) limit = 100 else {
    # retrieve posts before the last 7 days
    extra = paste0('&sort=top&&until=', format(Sys.time() - 7 * 24 * 3600, fmt))
  }
  u = sprintf(
    'https://bsky.social/xrpc/app.bsky.feed.searchPosts?q=%s&since=%s&limit=%d%s',
    utils::URLencode(query, reserved = TRUE), since, limit, extra
  )
  h = curl::new_handle()
  curl::handle_setheaders(h, Authorization = paste('Bearer', jwt))
  res = curl::curl_fetch_memory(u, handle = h)
  if (res$status_code != 200)
    stop('searchPosts failed (HTTP ', res$status_code, '): ', rawToChar(res$content))
  jsonlite::fromJSON(rawToChar(res$content))$posts
}

for (i in seq_len(NROW(m))) {
  q = m[i, 'query']
  s = search_bsky(q, m[i, 'since'], m[i, 'limit'])
  if (NROW(s) == 0) next
  s = s[!basename(s$uri) %in% ids, ]
  if (NROW(s) == 0) next
  s = s[order(s$likeCount, decreasing = TRUE), ]
  id = basename(s$uri); ids = c(ids, id)
  m[i, 'since'] = max(s$indexedAt)  # update 'since' for newer results next time
  h = s$author$handle
  b = s$record$text
  b = gsub('```', '\\`\\`\\`', b, fixed = TRUE)
  b = gsub('\\{\\{<(.*?)>}', '{{&lt;\\1&gt;}', b)  # show shortcode verbatim
  b = gsub('\\s*$', '', b)
  # use most liked post as title
  if (max(s$likeCount) > n_like) {
    t = xfun::strip_html(gsub('\n', ' ', b[which.max(s$likeCount)]))
    n_like = max(s$likeCount)
  }
  b = paste(b, sprintf(' [&#8618;](https://bsky.app/profile/%s/post/%s)', h, id))
  links = unlist(lapply(s$record$facets, function(fc) {
    u = unlist(lapply(fc$features, function(ft) {
      ft[ft[, '$type'] == 'app.bsky.richtext.facet#link', ]$uri
    }))
    if (length(u)) paste0('\n\n', paste0('- <', u, '>', collapse = '\n')) else ''
  }))
  x = c(
    x, paste('#', gsub(' .+', '', q)), '', paste0(
      '**', trimws(s$author$displayName), '** (@', h, '; ', s$likeCount, '/',
      s$replyCount, '/', s$repostCount, '; ', as.Date(s$indexedAt),
      '): ', b, links, collapse = '\n\n---\n\n'
    ), ''
  )
  Sys.sleep(1)
}

p = sprintf('content/post/%s.md', d)
if (length(x)) if (file.exists(p)) {
  cat(paste(c('', '', x, ''), collapse = '\n'), file = p, append = TRUE)
} else writeLines(
  c(jsonlite::toJSON(list(title = t, date = d), auto_unbox = TRUE, pretty = TRUE), '', x),
  p
)

write.csv(m[order(m$query), , drop = FALSE], f, row.names = FALSE)
