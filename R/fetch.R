dir.create('content/post', showWarnings = FALSE)
d = Sys.Date()

if (!require('xfun')) install.packages('xfun')
xfun::pkg_load2('jsonlite')

xfun:::pkg_update()

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
    'https://api.bsky.app/xrpc/app.bsky.feed.searchPosts?q=%s&since=%s&limit=%d%s',
    query, since, limit, extra
  )
  x = xfun::read_utf8(u)
  jsonlite::fromJSON(x)$posts
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
