const fs = require('fs');
const path = require('path');
const configJsonPath = path.resolve(__dirname, './config.json');
const config = JSON.parse(fs.readFileSync(configJsonPath, 'utf8'));
const s3 = require('s3');
const s3diff = require('s3-diff');

function doDiff(bucket) {
  return new Promise((resolve, reject) => {
    s3diff({
      aws: {
        apiVersion: '2006-03-01',
        region: config.region
      },
      local: __dirname + '/dest/public',
      remote: {
        bucket: bucket
      },
      recursive: false
    }, function(e, data) {
      if (e) {
        reject(e)
      } else {
        resolve(data);
      }
    });
  });
}

doDiff(config.s3Bucket).then(data => {
  console.log(data);
}).catch(e => {
  console.error(e);
  process.exit(1);
});
