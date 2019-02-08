const fs = require('fs');
const path = require('path');
const AWS = require('aws-sdk');
const gzip = require('gzip-js');

const env = process.argv[2];
const rootDir = __dirname + '/..';
const publicDir = rootDir + '/dest/public';
const configJsonPath = rootDir + `/config.${env}.json`;
const config = JSON.parse(fs.readFileSync(configJsonPath, 'utf8'));

var s3 = new AWS.S3({
  apiVersion: '2006-03-01',
  region: config.region
});

fs.readdirSync(publicDir).map(file => {
  var options = {
    level: 6,
    name: file,
    timestamp: parseInt(Date.now() / 1000, 10)
  };
  var body = fs.readFileSync(publicDir + '/' + file);
  if (!file.endsWith('.pdf')) {
    body = gzip.zip(body, options);
  }
  return {
    file: file,
    body: new Buffer(body)
  };
}).map(result => {
  var file = result.file;
  var body = result.body;
  var contentType;
  if (file.endsWith('.html') || file === 'login' || file === 'master' || file === 'doc') {
    contentType = 'text/html';
  } else if (file.endsWith('.js')) {
    contentType = 'application/javascript';
  } else if (file.endsWith('.css')) {
    contentType = 'text/css';
  } else if (file.endsWith('.pdf')) {
    contentType = 'application/pdf';
  } else if (file.endsWith('.png')) {
    contentType = 'image/png';
  }

  var contentEncoding;
  if (!file.endsWith('.pdf')) {
    contentEncoding = 'gzip';
  } else {
    contentEncoding = undefined;
  }

  return {
    Bucket: config.s3Bucket,
    Key: file,
    Body: body,
    ContentType: contentType,
    ContentEncoding: contentEncoding
  };
}).map(options => {
  return upload(options);
}).reduce((memo, p) => {
  return memo.then(_ => p);
}, Promise.resolve()).then(_ => {
  console.log('done');
}).catch(e => {
  console.error(e);
  process.exit(1);
});

function fixContentTypeToHtml(bucket, key) {
  return new Promise((resolve, reject) => {
    s3.copyObject({
      Bucket: bucket,
      CopySource: `${bucket}/${key}`,
      Key: key,
      MetadataDirective: "REPLACE",
      ContentType: "text/html"
    }, function(e) {
      if (e) {
        reject(e);
      } else {
        resolve();
      }
    });
  });
}

function upload(options) {
  return new Promise((resolve, reject) => {
    s3.upload(options, function(e) {
      if (e) {
        reject(e);
      } else {
        resolve();
      }
    });
  });
}
