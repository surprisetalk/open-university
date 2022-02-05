const fs = require(`fs`);
const path = require(`path`);
const crypto = require('crypto');

const glob = require(`glob`);

const Handlebars = require(`handlebars`);

const express = require(`express`);
const app = express();

const randomString = () => (Math.random() + 1).toString(36).substring(7);

const md5 = x => crypto.createHash(`md5`).update(x).digest(`hex`);

app.use(require(`cors`)())

app.use(express.text())
app.use(express.json())

app.use(express.static(`public`));

const isDir = fileName =>
  fs.existsSync(fileName)
  && fs.lstatSync(fileName).isDirectory();

const crawlLessons = fileName => {
  const children = !isDir(fileName)
    ? undefined
    : fs.readdirSync(fileName)
      .map(fileName_ => path.join(fileName,fileName_))
      .map(crawlLessons)
      .filter(x=>x);
  const parent = {
    title: path.parse(fileName).name.replace(/^\d+ /,``),
    hash: !isDir(fileName)
      ? md5(fs.readFileSync(fileName))
      : md5(children.map(x => x.hash).join(``)),
    lessons: children,
  };
  switch (path.parse(fileName).ext) {
    case `.md`: parent.type = `guide`; break;
    case `.sh`: parent.type = `puzzle`; break;
    case ``: parent.type = `lesson`; break;
    default: return undefined;
  }
  return parent;
};

const lessons = fs.readdirSync(`${__dirname}/lessons`)
  .map(fileName => path.join(__dirname,`lessons`,fileName))
  .map(crawlLessons);

// console.log(`\n`,`LESSONS`, JSON.stringify(lessons,true,2));

app.get(`/api/lessons`, (req, res) => {
  res.json(lessons);
});

const guides = glob.sync(`${__dirname}/lessons/*/**/*.md`)
  .map(fileName => ({
    title: path.parse(fileName).name.replace(/^\d+ /,``),
    content: Handlebars.compile(fs.readFileSync(fileName,`utf8`))({ params: { vars: { name: `Linda` } } }),
    hash: md5(fs.readFileSync(fileName)),
  }))
  .reduce((acc,val) => { acc[val.hash] = val; return acc; }, {});

// console.log(`\n`,`GUIDES`, JSON.stringify(guides,true,2));

app.get(`/api/guide/:hash`, (req, res) => {
  res.json(guides[req.params.hash]);
});

const puzzles = glob.sync(`${__dirname}/lessons/*/**.sh`)
  .map(fileName => ({
    title: path.parse(fileName).name.replace(/^\d+ /,``),
    hash: md5(fs.readFileSync(fileName)),
    path: fileName,
  }))
  .reduce((acc,val) => { acc[val.hash] = val; return acc; }, {});

// console.log(`\n`,`PUZZLES`, JSON.stringify(puzzles,true,2));

const openPuzzles = {};

const pastPuzzles = {};

app.post(`/api/puzzle/:hash`, (req, res) => {
  if (req.body === undefined)
    throw new Error(`Request body is undefined.`);
  else if (req.body === null || req.body === '' || Object.keys(req.body).length === 0) {
    openPuzzles[req.params.hash] = openPuzzles[req.params.hash]
      ?? JSON.parse(require(`child_process`).execSync(`'${puzzles[req.params.hash].path}'`))
        .map(({question,choices,solution}) => ({question,choices,solution}))
        .map(({question}) => ({question: Handlebars.compile(fs.readFileSync(fileName,`utf8`))({ params: { vars: { name: `Linda` } } })}));
  } else {
    for (const i in req.body)
      openPuzzles[req.params.hash][i].guess = req.body[i];
    pastPuzzles[req.params.hash] =
      [ openPuzzles[req.params.hash] ?? [] ]
        .concat(pastPuzzles[req.params.hash] ?? []);
    openPuzzles[req.params.hash] = undefined;
  }
  res.status(204).send();
});

app.get(`/api/puzzle/:hash`, (req, res) => {
  res.json({
    ...puzzles[req.params.hash],
    current: openPuzzles[req.params.hash]
      ? openPuzzles[req.params.hash].map(x => ({...x,solution:null}))
      : null,
    history: pastPuzzles[req.params.hash] ?? [],
    path: undefined,
  });
});

app.use((req,res,next) => {
  res.sendFile(path.join(__dirname,`public`,`index.html`));
});

app.use((err, req, res, next) => {
  console.error(err);
  res.status(500).send({error: err});
});

app.listen(process.env.PORT ?? 3000);
