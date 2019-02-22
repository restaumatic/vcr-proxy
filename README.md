# VCR proxy

VCR proxy can be used  to record and replay recorded API interactions.

It is not a HTTP proxy in the normal sense, instead the HTTP client which we are testing should be pointed (if possible) at the proxy as its endpoint. VCR proxy in Record mode will use the "endpoint" argument to route the requests to the remote API.


## Usage

```
% vcr-proxy --help
VCR Proxy

Usage: vcr-proxy (-c|--cassette CASSETTE_FILE) [-r|--record REMOTE_API_ENDPOINT]
                 [--port INT]
  Run the VCR proxy to replay or record API calls. Runs in replay mode by
  default.

Available options:
  -c,--cassette CASSETTE_FILE
                           Cassette yaml file for recording/replaying the API
                           interactions
  -r,--record REMOTE_API_ENDPOINT
                           Run in record mode, and forward requests to the
                           specified API endpoint
  --port INT               Port to listen on (default: 3128)
  -h,--help                Show this help text
```

### Example usage

1. Run vcr proxy in `Record` mode:

```
vcr-proxy -r "https://postman-echo.com/" -c cassette.yaml
```

2. Interact with the "API"

```
curl --request POST "http://localhost:3128/post" --data "RETURN THIS"  --verbose
```

3. The request should be recorded in the `cassette.yaml` file

```
ignoredHeaders: []
apiCalls:
- response:
    status:
      code: 200
      message: OK
    body: ! '{"args":{},"data":"","files":{},"form":{"RETURN THIS":""},"headers":{"x-forwarded-proto":"https","host":"postman-echo.com","content-length":"11","accept":"*/*","accept-encoding":"gzip","content-type":"application/x-www-form-urlencoded","user-agent":"curl/7.54.0","x-forwarded-port":"443"},"json":{"RETURN
      THIS":""},"url":"https://postman-echo.com/post"}'
    headers:
    - - x-via-proxy
      - 'yes'
    - - content-encoding
      - gzip
    - - content-type
      - application/json; charset=utf-8
    - - date
      - Wed, 20 Feb 2019 12:46:04 GMT
    - - etag
      - W/"161-/+7sogff3HHwvNZ0N6/PEIi992Y"
    - - server
      - nginx
    - - set-cookie
      - sails.sid=s%3AZ8Er_mJ6ZdZGpprlP86lQ6gzxEK6K881.WOKz1JtNAc9jfzuTdaIy4jo6vCrLT0dQCY9ykUdG%2FEU;
        Path=/; HttpOnly
    - - vary
      - Accept-Encoding
    - - content-length
      - '237'
    - - connection
      - keep-alive
  request:
    methodName: POST
    body: RETURN THIS
    url: /post
    params: []
    headers: []
```

4. Run the proxy in `Replay` mode

```
vcr-proxy -c cassette.yaml
```

5. Interact with the "API"

```
curl --request POST "http://localhost:3128/post" --data "RETURN THIS"  --verbose
```

The request recorded in the cassette file should be returned !


### Why not a normal proxy?

We wouldn't be able to handle HTTPS / CONNECT requests.


### TODO:

I didn't really think through what should be the "endpoint" parameter passed as an argument, ie. should it be just `scheme://host:port/` or should it also include `path`.

