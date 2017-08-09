const fs = require('fs');
const path = require('path');
const AWS = require('aws-sdk');
const configJsonPath = path.resolve(__dirname, './config.json');
const config = JSON.parse(fs.readFileSync(configJsonPath, 'utf8'));
const s3 = require('s3');

var s3Options = {
  apiVersion: '2006-03-01',
  region: config.region
};

var originalS3 = new AWS.S3(s3Options);

var client = s3.createClient({
  maxAsyncS3: 20,
  s3RetryCount: 3,
  s3RetryDelay: 1000,
  multipartUploadThreshold: 20971520,
  multipartUploadSize: 15728640,
  s3Options: s3Options
});

upload(config.s3Bucket, 'dest/public').then(_ => {
  console.log('done');
}).then(_ => {
  return fixContentTypeToHtml(config.s3Bucket, 'upload/test/login').then(_ => {
    return fixContentTypeToHtml(config.s3Bucket, 'upload/test/master');
  });
}).catch(e => {
  console.error(e);
  process.exit(1);
});

function fixContentTypeToHtml(bucket, key) {
  return new Promise((resolve, reject) => {
    originalS3.copyObject({
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

function upload(bucket, dir) {
  return new Promise((resolve, reject) => {
    var uploader = client.uploadDir({
      localDir: dir,
      deleteRemoved: false,
      s3Params: {
        Bucket: bucket,
        Prefix: "upload/test/"
      },
    });
    uploader.on('error', function(e) {
      reject(e);
    });
    uploader.on('progress', function() {
      console.log("progress", uploader.progressAmount, uploader.progressTotal);
    });
    uploader.on('end', function() {
      resolve();
    });
  });
}
