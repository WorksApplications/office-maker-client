import { createServer } from './create-test-server';
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

beforeAll(async () => {
  app = await createServer(30300);
  await page.setCacheEnabled(false);
});

afterAll(async () => {
  app.close();
});

describe('Open floor page', () => {
  beforeEach(async () => {
    await page.goto('http://localhost:30300');
    await page.waitForNavigation();
  });

  it('should match the image snapshot', async () => {
    const image = await page.screenshot();
    expect(image).toMatchImageSnapshot();
  });

  it('should display "Office Maker (test)" text on page', async () => {
    await expect(page).toMatch('Office Maker (test)');
  });
});
