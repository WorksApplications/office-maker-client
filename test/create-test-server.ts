// See ../watch.js
import { Server } from 'http';
import express from 'express';
import * as path from 'path';
import * as child_process from 'child_process';

const publicDir = path.resolve(__dirname, '../dest/public');
const buildScriptPath = path.resolve(__dirname, '../build.sh');

export const createServer: () => Promise<Server> = () => {
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

  return new Promise((resolve, reject) => {
    child_process.execSync(`${buildScriptPath} test`, { shell: 'true' });

    const server = app.listen(3030, () => {
      resolve(server);
    });
  });
};
