const fs = require('fs');
const path = require('path');
const configJsonPath = path.resolve(__dirname, './config.json');
const config = JSON.parse(fs.readFileSync(configJsonPath, 'utf8'));
const s3 = require('s3');

var client = s3.createClient({
  maxAsyncS3: 20,
  s3RetryCount: 3,
  s3RetryDelay: 1000,
  multipartUploadThreshold: 20971520,
  multipartUploadSize: 15728640,
  s3Options: {
    apiVersion: '2006-03-01',
    region: config.region
  },
});

upload(config.s3Bucket, 'dest/public').then(_ => {
  console.log('done');
}).catch(e => {
  console.error(e);
  process.exit(1);
});

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
