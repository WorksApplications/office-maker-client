const fs = require('fs');
const path = require('path');
const AWS = require('aws-sdk');
const configJsonPath = path.resolve(__dirname, './config.json');
const config = JSON.parse(fs.readFileSync(configJsonPath, 'utf8'));
const gzip = require('gzip-js');

var s3 = new AWS.S3({
  apiVersion: '2006-03-01',
  region: config.region
});

fs.readdirSync('dest/public').map(file => {
  var options = {
    level: 3,
    name: file,
    timestamp: parseInt(Date.now() / 1000, 10)
  };
  var body = gzip.zip(fs.readFileSync(__dirname + '/dest/public/' + file), options);
  return {
    file: file,
    body: new Buffer(body)
  };
}).map(result => {
  var file = result.file;
  var body = result.body;
  var contentType;
  if (file.endsWith('.html') || file === 'login' || file === 'master') {
    contentType = 'text/html'
  } else if (file.endsWith('.js')) {
    contentType = 'application/javascript'
  } else if (file.endsWith('.css')) {
    contentType = 'text/css'
  }
  return {
    Bucket: config.s3Bucket,
    Key: file,
    Body: body,
    ContentType: contentType,
    ContentEncoding: 'gzip'
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
