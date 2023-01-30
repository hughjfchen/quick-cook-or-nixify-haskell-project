# {{name|toPascal}} :: Web Api Template

[![Hackage](https://img.shields.io/hackage/v/{{name}}.svg)](https://hackage.haskell.org/package/{{name}}) [![Build Status](https://github.com/cackharot/{{name}}/workflows/{{name|toPascal}}%20CI/badge.svg)](https://github.com/cackharot/{{name}}/actions?query=workflow%3A%22{{name|toPascal}}+CI%22) [![Hackage Deps](https://img.shields.io/hackage-deps/v/{{name}}.svg)](http://packdeps.haskellers.com/reverse/{{name}})

A REST web api server template in Haskell.

This serves as a base to create API projects that comes with the following features to avoid repeated bolierplate in each one.

- Light weight & super fast Warp http server
- Supports HTTP 1.1 & HTTP/2 TLS by warp
- JSON as input/output serialization using Aeson library
- Structured logging (JSON) both application & HTTP request
- Define the REST interface using Servant library
- JWT (Bearer) token based authentication
- Application config via ENVIRONMENT variables (via dotenv & envy)
- Health & Info endpoints
- Prometheus metrics endpoint

## TODO

- [x] Integrate with RIO
- [x] Integrate with Servant
- [x] Integrate with FastLogger
- [x] Integrate with doenv & envy
- [x] Integrate with Prometheus
- [x] Integrate with wai-util
- [x] Setup JWT Authentication
- [x] JSON Error formatting
- [x] Setup HTTPS
- [ ] Setup PSQL/SqlLite pool with Presistent
- [x] Setup Stack template

## Getting started

To create a bare minimum API service all you need is below:

```haskell
#!/usr/bin/env stack
{- stack --resolver lts-18.14 runghc
 --package {{name}}
-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators #-}
import RIO
import {{name|toPascal}}
import Servant

type HelloRoute = "hello" :> QueryParam "name" Text :> Get '[PlainText] Text
type API = HelloRoute :<|> EmptyAPI

hello :: Maybe Text -> BasicApp Text
hello name = do
  let name' = fromMaybe "Sensei!" name
  logInfo $ "Saying hello to " <> display name'
  return $ "Hello " <> name' <> "!"

main :: IO ()
main = do
  let infoDetail = InfoDetail "example" "dev" "0.1" "change me"
      appEnv = appEnvironment infoDetail
      appVer = appVersion infoDetail
      appAPI = Proxy :: Proxy API
      appServer = hello :<|> emptyServer
  logFunc <- buildLogger appEnv appVer
  middlewares <- {{name}}Middlewares infoDetail
  run{{name|toPascal}}AppWithMetrics
    middlewares
    EmptyContext
    (logFunc, infoDetail)
    appAPI
    appServer
```

### Usage

Add this package to your application and refer `examples` directory for inspiration.

```bash
# Create a new haskell app using stack's rio template
stack new UserApi rio
```

Edit the below files to include the dependencies

```yaml
# Stack package.yaml
dependencies:
- base >= 4.11 && < 10
- {{name}}
- rio
- servant-server

# stack.yaml
resolver: lts-18.14
packages:
- .
extra-deps:
- {{name}}-0.1.2
- wai-cli-0.2.3
```

A stack project template is also available for bootstraping quickly

```bash
stack new UserApi https://raw.githubusercontent.com/cackharot/{{name}}/main/{{name}}.hsfiles
```

## Build & Run

```bash
make build
PORT=3000 make run

open http://localhost:3000/health
```

### Endpoints

Info: http://localhost:3000/info

Health: http://localhost:3000/health

Metrics: http://localhost:3000/metrics

## Run tests

```bash
make test
```

## HTTPS Setup

Generate RootCA & localhost Public & Private key pair

Be sure to edit `certs/domains.ext` file if you need more DNS aliases before executing these commands.

```bash
openssl req -x509 -nodes -new -sha256 -days 1024 -newkey rsa:2048 -keyout "certs/RootCA.key" -out "certs/RootCA.pem" -subj "/C=US/CN=Localhost-Root-CA"
openssl x509 -outform pem -in "certs/RootCA.pem" -out "certs/RootCA.crt"

openssl req -new -nodes -newkey rsa:2048 -keyout "certs/localhost.key" -out "certs/localhost.csr" -subj "/C=US/ST=NoWhere/L=NoWhere/O=Localhost-Certificates/CN=localhost.local"
openssl x509 -req -sha256 -days 1024 -in "certs/localhost.csr" -CA "certs/RootCA.pem" -CAkey "certs/RootCA.key" -CAcreateserial -extfile "certs/domains.ext" -out "certs/localhost.crt"
```

Once you have these keys then to start the server in https run the below command:

```bash
/path/to/urapiexec --port 3443 --protocol http+tls --tlskey certs/localhost.key --tlscert certs/localhost.crt
```

open https://localhost:3443/health

## JWT Authentication

This template supports only authentication against standard JWT issued by Azure, Okta, Keybase & other OAuth JWT authentication providers.
However its easily customizable to authenticate against custom issued JWT token. This template does not issue a JWT token.

The validate any incoming JWT's we need the public keys (provider by JWT issuer) to verify the payload and also need to verify the audiences.
These are configurable via ENV variables

```bash
JWK_PATH="secrets/jwk.sig"
JWK_AUDIENCES="api://some-resource-id"
```
