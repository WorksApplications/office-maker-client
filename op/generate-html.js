const fs = require('fs');
const ejs = require('ejs');

const env = process.argv[2];
const rootDir = __dirname + '/..';
const configJsonPath = rootDir + `/config.${env}.json`;
const config = JSON.parse(fs.readFileSync(configJsonPath, 'utf8'));

const publicDir = rootDir + '/dest/public';
const templateDir = rootDir + '/src/template';

generate(config, templateDir + '/index.html', publicDir + '/index.html');
generate(config, templateDir + '/login.html', publicDir + '/login');
generate(config, templateDir + '/master.html', publicDir + '/master');
console.log('generated html.');

function generate(config, src, dest) {
  const html = ejs.render(fs.readFileSync(src, 'utf8'), config);
  fs.writeFileSync(dest, html);
}
