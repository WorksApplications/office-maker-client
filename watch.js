const cp = require('child_process');
const watch = require('watch');
const path = require('path');
const minimatch = require('minimatch');
var express = require('express');

function runServer() {
  var app = express();
  app.use((req, res, next) => {
    if (req.url.indexOf('/login') >= 0 || req.url.indexOf('/master') >= 0) {
      res.set({
        'Content-Type': 'text/html'
      });
    }
    next();
  });
  app.use(express.static(__dirname + '/dest/public'));

  app.listen(3000, function() {
    console.log('Example app listening on port 3000!')
  })
}


var debugMode = process.argv.filter(a => {
  return a == '--debug'
}).length;

var queued = {
  build: false
};

function taskBuild(cb) {
  if (queued.build) {
    queued.build = false;
    // console.log('build start\n');
    var args = ['build.sh'];
    if (debugMode) {
      args.push('--debug');
    }
    var sh = cp.spawn('sh', args, {
      stdio: 'inherit'
    });
    sh.on('close', cb);
  } else {
    cb();
  }
}

function runWatcher() {
  taskBuild(() => {
    setTimeout(runWatcher, 300);
  });
}

function schedule(type, stat) {
  queued[type] = true;
}
watch.createMonitor('src', (monitor) => {
  monitor.on("created", schedule.bind(null, 'build'));
  monitor.on("changed", schedule.bind(null, 'build'));
  monitor.on("removed", schedule.bind(null, 'build'));
});


schedule('build');
runWatcher();
runServer();
