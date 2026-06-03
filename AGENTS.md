# emacs-bluesky Agent Notes

This file captures project context that is not obvious from the code alone.
Prefer these sources and constraints when extending the client.

## Source Of Truth

- AT Protocol overview: https://docs.bsky.app/docs/advanced-guides/atproto
- Posts guide: https://docs.bsky.app/docs/advanced-guides/posts
- Creating posts tutorial: https://docs.bsky.app/docs/tutorials/creating-a-post
- Thread gates tutorial: https://docs.bsky.app/docs/tutorials/thread-gates
- Viewing threads tutorial: https://docs.bsky.app/docs/tutorials/viewing-threads
- Likes and reposts tutorial: https://docs.bsky.app/docs/tutorials/like-repost
- API reference root: https://docs.bsky.app/docs/api
- Canonical lexicons: https://github.com/bluesky-social/atproto/tree/main/lexicons
- Post lexicon, including current text limits: https://raw.githubusercontent.com/bluesky-social/atproto/main/lexicons/app/bsky/feed/post.json
- Postgate lexicon: https://raw.githubusercontent.com/bluesky-social/atproto/main/lexicons/app/bsky/feed/postgate.json

Use official Bluesky docs or canonical lexicons for protocol behavior. SDK docs
and third-party references can be useful for examples, but should not override
the lexicons.

## Architecture Notes

- `bluesky-conn.el` should hold XRPC and repository-write helpers.
- `bluesky-ui.el` should render app-view records and embeds.
- `bluesky.el` should own timeline/thread buffers, navigation, selection, and
  keybindings.
- `bluesky-post.el` should own compose buffers, post/reply submission,
  character counting, rich-text conversion, and post/thread gate options.
- Do not put posting UI back into minibuffer prompts. Posts and replies should
  go through `bluesky-post-mode`.
- Timeline reply context should preserve conversation structure. If an ancestor
  post has already been rendered, later loaded descendants should be attached
  under that original ancestor rather than shown as an unrelated adjacent item.
- Newly loaded timeline posts should use the `bluesky-new-post` background only
  for the most recent load. Loading or refreshing another page should clear the
  previous new-post background and mark only the newest additions.

## AT Protocol Model

- User data is stored as signed repository records. App views return convenient
  view objects, counts, and `viewer` state; repo writes still go through
  `com.atproto.repo.createRecord` and related repository APIs.
- DIDs and AT URIs are stable record identifiers. Handles can change; use AT
  URI plus CID strong refs when replying, quoting, liking, and reposting.
- Lexicons define schema and behavior. If behavior is unclear from docs, inspect
  the raw lexicon file in the `bluesky-social/atproto` repository.
- Atproto separates speech from reach. Moderation labels and app-view `viewer`
  flags should affect rendering and UI affordances, not be treated as core repo
  record data.

## Posts And Replies

- `app.bsky.feed.post` records require `text` and `createdAt`.
- The post lexicon currently limits post text to `maxGraphemes: 300` and
  `maxLength: 3000` bytes. The compose header should show both and block
  submission when over either limit.
- Replies require a `reply` object containing both `root` and `parent` strong
  refs. `parent` is the post being replied to; `root` is the original thread
  root, or the parent itself for a top-level reply.
- If `post.viewer.replyDisabled` is true, do not open a reply composer and do
  not submit a reply if a composer somehow already exists.
- Validate reply-disabled behavior against live app-view objects, but do not
  actually submit replies unless the user explicitly asks for a live write.

## Rich Text Facets

- Facets annotate text ranges using UTF-8 byte offsets, not Emacs character
  positions.
- Current `bluesky-post.el` conversion supports plain URLs and simple Markdown
  or Org links as link facets. It does not yet fully implement mention
  resolution or full Markdown/Org export.
- Mentions need handle-to-DID resolution via
  `com.atproto.identity.resolveHandle`; if resolution fails, leave the text
  un-faceted.
- When adding richer conversion, preserve the final text exactly as submitted
  and compute facet byte offsets from that final text.

## Gates And Interaction Options

- Thread gates control who can reply. Create an
  `app.bsky.feed.threadgate` record with the same rkey as the post.
- Threadgate allow rules include:
  - `app.bsky.feed.threadgate#mentionRule`
  - `app.bsky.feed.threadgate#followingRule`
  - `app.bsky.feed.threadgate#followerRule`
  - `app.bsky.feed.threadgate#listRule`
- A missing threadgate means anyone can reply. An empty `allow` vector means
  nobody can reply.
- Post gates control embedding. Create an `app.bsky.feed.postgate` record with
  the same rkey as the post. `app.bsky.feed.postgate#disableRule` disables
  embedding.

## Embeds And Rendering

- Feed app-view embed shapes differ from repo record embed shapes. Render view
  shapes such as `app.bsky.embed.record#view`,
  `app.bsky.embed.recordWithMedia#view`, `app.bsky.embed.video#view`, and
  image/external view records.
- Quote records should render inline inside the parent embed. Do not flatten
  quoted records into separate timeline items, or quotes will appear twice.
- `app.bsky.embed.record#view` can wrap a `:record` union. Unwrap it before
  deciding whether the inner record is a normal quoted post, blocked, not found,
  or detached.
- Blob URLs can be fetched through `com.atproto.sync.getBlob` with `did` and
  `cid`, but app views often already provide CDN thumbnail/fullsize URLs.

## Emacs UX Expectations

- `bluesky-mode` keybindings should be standard top-level `define-key` forms
  on sparse keymaps, so reloading the file updates live Emacs sessions.
- `r` opens a reply composer, `n` opens a new post composer, `o` opens links or
  media from the selected post, and `RET` opens a thread view.
- `bluesky-post-mode` should show its important commands and counters in the
  header line. `C-c C-c` posts, `C-c C-k` cancels.
- Reply compose buffers should show a read-only context overlay at the top with
  the post being replied to.
- Avoid minibuffer text entry for post creation. Users need visible counts,
  options, and context while writing.

## Testing And Safety

- It is fine to inspect live app-view data through Anvil or `emacsclient`.
- Do not perform live mutating actions such as posting, replying, liking,
  reposting, bookmarking, or deleting records unless the user explicitly asks.
- For validation, prefer constructing records/facets locally and loading files
  into the running Emacs session.
- `emacs -Q --batch` may not find this user's package dependencies. When that
  happens, validate by loading the files in the running Emacs daemon.
- For Org files, follow the user-level instruction in `~/AGENTS.md`: edit
  through Anvil or org-mcp so live Org buffers stay in sync.
