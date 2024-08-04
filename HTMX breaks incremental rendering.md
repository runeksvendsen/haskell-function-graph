## HTMX breaks incremental rendering

Consider an HTTP backend with an endpoint that delivers HTML to a frontend. This endpoint — let's use `/search?query=foo` as an example — delivers HTML that shows search results based on some query entered by the client (e.g. "*dog pictures*"). The search results take some time to retrieve for this backend, so it streams one result at a time over HTTP (using `Transfer-Encoding: chunked`). The first result arrives after 500 milliseconds, the second result after 1000 milliseconds, and so on and so forth until result number 10 is delivered after five seconds. So, this example backend will immediately respond with `<html><body><h1>Results</h1><ol>` and every 500 milliseconds a result of the form `<li><a href="todo">result n</a></li>` will be sent over the open HTTP connection. This will continue for five seconds seconds (ie. after 10 all results are delivered). Finally, the backend sends `</ol></body></html>` and closes the connection.

Here's the real-time output of *curl* when run against this example endpoint: 

![real-time curl output](https://github.com/user-attachments/assets/1abc3160-df76-40b3-a6ef-428e2900cfc7)

All browsers will render these results incrementally; as they come in [[1]](https://en.wikipedia.org/wiki/Incremental_rendering). The user will thus immediately see the **Results** header, the first search result will be displayed after 500 milliseconds, the second result within one second and so on and so forth. This is how the page loads in Chrome:

![loading the `search` endpoint in Chrome](https://github.com/user-attachments/assets/eaf6c350-c305-481b-815e-c8e07b1b2690)

If, however, HTMX is used for this endpoint with e.g. `hx-boost`, then nothing at all will be displayed until five seconds have passed (after all results have been sent by the backend and the connection has been closed). 

The following page has two links both to the `/search` endpoint: one regular (without `hx-boost`) and another with `hx-boost`. 

```html
<html>
  <head>
  	<script src="https://unpkg.com/htmx.org@2.0.1" integrity="sha384-QWGpdj554B4ETpJJC9z+ZHJcA/i59TyjxEPXiiUgN2WmTyV5OEZWCD6gQhgkdpB/" crossorigin="anonymous">			</script>
  </head>
  <body>
    <a href="/search?query=foo">Regular link to /search</a>
    <br>
    <br>
    <a href="/search?query=foo" hx-boost="true">Boosted link to /search</a>
  </body>
</html>
```

Here's how the two links load in Chrome:

![loading the `search` endpoint in Chrome via HTMX](https://github.com/user-attachments/assets/c6dbcfc7-31e6-49c3-b479-777dfc1dada0)

As you can see, content is displayed instantly for the regular, non-boosted link (first the **Results** header, followed by the first result after 500 milliseconds). The boosted link, however, appears broken, until five seconds pass and everything is shown at once. This essentially breaks HTMX web pages for endpoints that are too slow to deliver the final byte to the client fast enough, as HTMX currently waits for the connection to be closed before displaying anything at all. This impacts slow connections as well, thereby hurting the responsiveness of HTMX on e.g. mobile connections.

A feature PR was opened (https://github.com/bigskysoftware/htmx/pull/2101) which was unfortunately rejected by a maintainer of HTMX. An issue has been opened (https://github.com/bigskysoftware/htmx/issues/1911) which suggests that this is a feature that HTMX should have. I'm creating this issue because I think it's reasonable to consider this a bug in HTMX. A user adding a `hx-boost` attribute to an `a` tag should expect to see the same result as if there were no such attribute, but in the above case the result is radically different (instant response versus a five-second lag). The longer it takes for the backend to deliver the final byte the more broken this behavior is.

I'm opening this issue to discuss my stance above (that this is a bug). I think streaming HTML is the most natural and simple way to deliver results in a streaming fashion to a client while avoiding unnecessary Javascript libraries and frameworks, and I think it's a shame that it doesn't work with HTMX. There may be something I'm missing, however, in which case I'd like to have a discussion about it.