const cp = require('child_process');
const watch = require('watch');
const path = require('path');
const minimatch = require('minimatch');

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

function run() {
  taskBuild(() => {
    setTimeout(run, 300);
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
run();
