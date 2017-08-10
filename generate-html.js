var fs = require('fs');
var path = require('path');
var ejs = require('ejs');
var configJsonPath = path.resolve(__dirname, './config.json');
var config = JSON.parse(fs.readFileSync(configJsonPath, 'utf8'));

var publicDir = __dirname + '/dest/public';

var outputFiles = {
  index: path.join(publicDir, 'index.html'),
  login: path.join(publicDir, 'login'),
  master: path.join(publicDir, 'master')
};

var templateDir = __dirname + '/src/template';
var indexHtml = ejs.render(fs.readFileSync(templateDir + '/index.html', 'utf8'), {
  apiRoot: config.apiRoot,
  accountServiceRoot: config.accountServiceRoot,
  imageRoot: config.imageRoot,
  title: config.title
});
fs.writeFileSync(outputFiles.index, indexHtml);

var loginHtml = ejs.render(fs.readFileSync(templateDir + '/login.html', 'utf8'), {
  accountServiceRoot: config.accountServiceRoot,
  title: config.title
});
fs.writeFileSync(outputFiles.login, loginHtml);

var masterHtml = ejs.render(fs.readFileSync(templateDir + '/master.html', 'utf8'), {
  apiRoot: config.apiRoot,
  accountServiceRoot: config.accountServiceRoot,
  title: config.title
});
fs.writeFileSync(outputFiles.master, masterHtml);

return outputFiles;
