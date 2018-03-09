if (process.argv.length < 3) {
    console.error('lack of 3rd argument.(connection to <prod>, <stg>, or <local>)');
    process.exit(1);
}

const cp = require('child_process');
const watch = require('watch');
const path = require('path');
const minimatch = require('minimatch');
const express = require('express');
const env = process.argv[2];
const rootDir = __dirname;
const publicDir = rootDir + '/dest/public';

function runServer() {
  const app = express();
  app.use((req, res, next) => {
    if (req.url.indexOf('/login') >= 0 || req.url.indexOf('/master') >= 0) {
      res.set({
        'Content-Type': 'text/html'
      });
    }
    next();
  });
  app.use(express.static(publicDir));
  app.listen(3030, _ => {
    console.log('Example app listening on port 3030!')
  });
}

const queued = {
  build: false
};

function taskBuild(cb) {
  if (queued.build) {
    queued.build = false;
    // console.log('build start\n');
    const args = [rootDir + '/build.sh', env];
    const sh = cp.spawn('sh', args, {
      stdio: 'inherit'
    });
    sh.on('close', cb);
  } else {
    cb();
  }
}

function runWatcher() {
  taskBuild(_ => {
    setTimeout(runWatcher, 300);
  });
}

function schedule(type, stat) {
  queued[type] = true;
}

watch.createMonitor('src', monitor => {
  monitor.on("created", schedule.bind(null, 'build'));
  monitor.on("changed", schedule.bind(null, 'build'));
  monitor.on("removed", schedule.bind(null, 'build'));
});

schedule('build');
runWatcher();
runServer();
