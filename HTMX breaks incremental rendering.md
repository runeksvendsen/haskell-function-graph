## HTMX breaks incremental rendering

Consider an HTTP backend with an endpoint that delivers HTML to a frontend, e.g. `/search?query=dog%20pictures`. This endpoint delivers HTML that shows search results based on some query entered by the client (e.g. "dog pictures"). The search results take some time to retrieve for this backend, so it streams one result at a time over HTTP (using `Transfer-Encoding: chunked`). The first result arrives after 500 milliseconds, the second result after 1000 milliseconds, and so on and so forth until result number 10 is delivered after five seconds. So, this example backend will immediately respond with `<html><body><ol>` and every 500 milliseconds a result of the form `<li><a href="todo">result n</a></li>` will be sent over the open HTTP connection. This will continue for five seconds seconds, ie. after 10 results are sent. Finally, the backend sends `</ol></body></html>` and closes the connection.

Here's the output of *curl* (in real-time) when run against this example endpoint: 

<img src="/Users/rune/curl.gif">

All browsers will render these results incrementally; as they come in [[1]](https://en.wikipedia.org/wiki/Incremental_rendering). The user will thus see the first result within 500 milliseconds, the second result within one second and so on and so forth. This is how the page loads in Chrome:

![browser-regular](/Users/rune/browser-regular.gif)

If, however, HTMX is used for this endpoint with e.g. `hx-boost`, then nothing at all will be displayed until 10 seconds have passed (after all results have been sent by the backend and the connection has been closed). This page has two links both to the `/search` endpoint: one regular (without `hx-boost`) and another with `hx-boost`. Here's how the two links load in Chrome:

```
TODO
```

As you can see, content is displayed almost instantly for the regular, non-boosted link while the boosted link is seemingly broken (doesn't load until after 100 seconds).

This essentially breaks HTMX web pages for endpoints that are too slow to deliver the final byte to the client fast enough, as HTMX currently waits for the connection to be closed before displaying anything at all. This impacts slow connections as well, thereby hurting the responsiveness of HTMX on e.g. mobile connections.

A feature PR was opened (https://github.com/bigskysoftware/htmx/pull/2101) which was unfortunately rejected by a maintainer of HTMX. An issue has been opened (https://github.com/bigskysoftware/htmx/issues/1911) which suggests that this is a feature that HTMX should have. I'm creating this issue because I think it's reasonable to consider this a bug in HTMX. A user adding a `hx-boost` attribute to an `a` tag should expect to see the same result as if there were no such attribute, but in the above case the result is radically different (it goes from usable to unusable).

I'm opening this issue to discuss my stance above (that this is a bug). I think streaming HTML is the most natural and simple way to deliver results in a streaming fashion to a client while avoiding unnecessary Javascript libraries and frameworks, and I think it's a shame that it doesn't work with HTMX. There may be something I'm missing, however, in which case I'd like to have a discussion about it.