// See ../watch.js
import { Server } from 'http';
import express, { Request, Response } from 'express';
import * as path from 'path';
import * as child_process from 'child_process';

const publicDir = path.resolve(__dirname, '../dest/public');
const buildScriptPath = path.resolve(__dirname, '../build.sh');

export const createElmServer = (port: number) => {
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

  return new Promise<Server>((resolve, reject) => {
    child_process.execSync(`sh ${buildScriptPath} test`);

    const server = app.listen(port, () => {
      resolve(server);
    });
  });
};

export const createMockServer = (
  port: number,
  apis: {
    request: {
      path: string;
      method: 'POST' | 'GET' | 'PUT' | 'DELETE' | 'OPTIONS';
    };
    response: {
      statusCode: number;
      body?: any;
      header?: { [key: string]: string };
    };
  }[],
  middleware?: (req: Request, res: Response, next: () => void) => void
) => {
  const app = express();
  if (middleware) {
    app.use(middleware);
  }

  apis.forEach(api => {
    app.use(api.request.path, (req, res, next) => {
      if (req.method == api.request.method) {
        res
          .status(api.response.statusCode)
          .set(api.response.header)
          .send(api.response.body);
      } else {
        next();
      }
    });
  });

  return new Promise<Server>((resolve, reject) => {
    const server = app.listen(port, () => {
      resolve(server);
    });
  });
};
