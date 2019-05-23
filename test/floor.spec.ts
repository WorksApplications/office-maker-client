import { createElmServer, createMockServer } from './create-test-server';
import { Server } from 'http';

declare global {
  namespace jest {
    interface Matchers<R> {
      toMatchImageSnapshot(): R;
    }
  }
}

const { toMatchImageSnapshot } = require('jest-image-snapshot');
expect.extend({ toMatchImageSnapshot });

let app: Server;
let mock: Server;

beforeAll(async () => {
  app = await createElmServer(30300);
  mock = await createMockServer(
    30400,
    [
      {
        request: {
          path: '/api/floors',
          method: 'GET'
        },
        response: {
          statusCode: 200,
          body: []
        }
      }
    ],
    (req, res, next) => {
      // CORS Settings
      res.header('Content-Type', 'application/json');
      res.header('access-control-allow-credentials', 'true');
      res.header('access-control-allow-origin', '*');

      if (req.method == 'OPTIONS') {
        res.header(
          'access-control-allow-headers',
          'Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token,X-Amz-User-Agent,Pragma,Cache-Control,If-Modified-Since'
        );
        res.header('access-control-allow-methods', 'OPTIONS,GET');
        res.sendStatus(200);
      } else {
        next();
      }
    }
  );
});

afterAll(async () => {
  app.close();
  mock.close();
});

describe('Open floor page', () => {
  beforeEach(async () => {
    await page.goto('http://localhost:30300');

    // why waitForNavigation does not work? (keep waiting forever)
    await page.waitFor(2000);
  });

  it('should match the image snapshot', async () => {
    const image = await page.screenshot();
    expect(image).toMatchImageSnapshot();
  });

  it('should display "Office Maker (test)" text on page', async () => {
    await expect(page).toMatch('Office Maker (test)');
  });
});
