Office Maker Client
====

Client implementation for Office Maker.

## Routing

|URL||
|:--|:--|
|/|top page|
|/#floorId|specific floor page|
|/?q=foo&edit=&object=#floorId|specific floor page with some conditions|
|/login|login page|
|/master|master page|


## Development Requirement

1. [Elm](http://elm-lang.org/) (>= 0.18)
2. [Node.js](https://nodejs.org/) (>= 6.0)

## Install

```
$ elm-package install
$ npm install
$ cp op/config-template.json config.dev.json
```

`config.${env}.json` is configuration for `dev` environment.

## Build

```
$ sh build.sh ${env}
```

- convert Elm code into JavaScript
- copy CSS files
- generate HTML


## Local Debug

```
$ node watch
```

This automatically runs `sh build.sh dev` if any file is changed. It also run static server on `http://localhost:3000`.

**Note:** You might need to disable CORS if server returns specific `Access-Control-Allow-Origin`.

Chrome 60 on Windows
```
$ start chrome http://localhost:3000 --disable-web-security --user-data-dir="%UserProfile%\AppData\Local\Google\Chrome\User Data"
```

Chrome 60 on Ubuntu
```
$ google-chrome http://localhost:3000 --disable-web-security --user-data-dir=/tmp/someDir
```

## Deploy

```
$ sh deploy.sh ${env}
```

## Configuration

Edit `config.${env}.json`. Bold properties are essential to work with various environment.

|name||
|:--|:--|
|title|This name will be used by header.|
|**accountServiceRoot**|URL of account service. http://xxx.xxx.xx.xx/accounts/api |
|**profileServiceRoot**|URL of profile service. http://xxx.xxx.xx.xx/profiles/api |
|**apiRoot**|URL of api server. http://xxx.xxx.xx.xx/api |
|**cacheRoot**|Cached objects are fetched from this URL. http://xxx.xxx.xx.xx/cache |
|**imageRoot**|Uploaded images are fetched from this URL. http://xxx.xxx.xx.xx/images |
|**region**|S3 region you want to deploy static files to. |
|**s3Bucket**|S3 bucket you want to deploy static files to. |


## Copyright

© 2017-Present WorksApplications CO.,LTD.


## License

[Apache License 2.0](LICENSE)
