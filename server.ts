const fs = require(`fs`);
const path = require(`path`);
const crypto = require('crypto');

const glob = require(`glob`);

const randomString = () => (Math.random() + 1).toString(36).substring(7);

const express = require(`express`);
const app = express();
const bodyParser = require('body-parser');

const md5 = x => crypto.createHash(`md5`).update(x).digest(`hex`);

app.use(require(`cors`)())

app.use(bodyParser.json());

app.use(express.static('public'));

const crawlLessons = fileName => {
  const children = !!path.extname(fileName)
    ? undefined
    : fs.readdirSync(fileName)
      .map(fileName_ => path.join(fileName,fileName_))
      .map(crawlLessons);
  const parent = {
    title: path.parse(fileName).name.replace(/^\d+ /,``),
    hash: !!path.extname(fileName)
      ? md5(fs.readFileSync(fileName))
      : md5(children.map(x => x.hash).join(``)),
    lessons: children,
  };
  switch (path.parse(fileName).ext) {
    case `.md`: parent.type = `guide`; break;
    case `.sh`: parent.type = `puzzle`; break;
    case ``: parent.type = `lesson`; break;
    default: throw new Error(fileName);
  }
  return parent;
};

const lessons = fs.readdirSync(`${__dirname}/lessons`)
  .map(fileName => path.join(__dirname,`lessons`,fileName))
  .map(crawlLessons);

console.log(`\n`,`LESSONS`, JSON.stringify(lessons,true,2));

app.get(`/lessons`, (req, res) => {
  res.json(lessons);
});

const guides = glob.sync(`${__dirname}/lessons/*/**.md`)
  .map(fileName => ({
    title: path.parse(fileName).name.replace(/^\d+ /,``),
    content: fs.readFileSync(fileName,`utf8`),
    hash: md5(fs.readFileSync(fileName)),
  }))
  .reduce((acc,val) => { acc[val.hash] = val; return acc; }, {});

console.log(`\n`,`GUIDES`, JSON.stringify(guides,true,2));

app.get(`/guide/:hash`, (req, res) => {
  res.json(guides[req.params.hash]);
});

const puzzles = glob.sync(`${__dirname}/lessons/*/**.sh`)
  .map(fileName => ({
    title: path.parse(fileName).name.replace(/^\d+ /,``),
    hash: md5(fs.readFileSync(fileName)),
    path: fileName,
  }))
  .reduce((acc,val) => { acc[val.hash] = val; return acc; }, {});

console.log(`\n`,`PUZZLES`, JSON.stringify(puzzles,true,2));

const openPuzzles = {};

const pastPuzzles = {};

app.put(`/puzzle/:hash`, (req, res) => {
  openPuzzles[req.params.hash] = openPuzzles[req.params.hash] ?? JSON.parse(require(`child_process`).execSync(`'${puzzles[req.params.hash].path}'`));
  res.status(204).send();
});

app.post(`/puzzle/:hash`, (req, res) => {
  pastPuzzles[req.params.hash] =
    (openPuzzles[req.params.hash].map(x => ({ ...x, guess: `TODO` })) ?? [])
      .concat(pastPuzzles[req.params.hash]);
  openPuzzles[req.params.hash] = undefined;
  res.status(204).send();
});

app.get(`/puzzle/:hash`, (req, res) => {
  res.json({
    ...puzzles[req.params.hash],
    current: openPuzzles[req.params.hash]
      ? openPuzzles[req.params.hash].map(x => { x.answer = null; return x; })
      : null,
    history: pastPuzzles[req.params.hash] ?? [],
    path: undefined,
  });
});

app.use((err, req, res, next) => {
  console.error(err);
  res.status(500).send({error: err});
});

app.listen(process.env.PORT ?? 3000);
