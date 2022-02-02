const fs = require(`fs`);
const path = require(`path`);
const crypto = require('crypto');

const glob = require(`glob`);

const randomString = () => (Math.random() + 1).toString(36).substring(7);

const app = require(`express`)();
const bodyParser = require('body-parser');

const md5 = x => crypto.createHash(`md5`).update(x).digest(`hex`);

app.use(bodyParser.json());

const crawlLessons = fileName => {
  const body = !!path.extname(fileName)
    ? []
    : fs.readdirSync(fileName)
      .map(fileName_ => path.join(fileName,fileName_))
      .map(crawlLessons);
  const lesson = {
    title: path.parse(fileName).name.replace(/^\d+ /,``),
    hash: !!path.extname(fileName)
      ? md5(fs.readFileSync(fileName))
      : md5(body.map(x => x.hash).join(``)),
    body,
  };
  switch (path.parse(fileName).ext) {
    case `.md`: lesson.type = `guide`; break;
    case `.sh`: lesson.type = `puzzle`; break;
    case ``: lesson.type = `lesson`; break;
    default: throw new Error(fileName);
  }
  return lesson;
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
    type: `guide`,
    title: path.parse(fileName).name.replace(/^\d+ /,``),
    body: fs.readFileSync(fileName,`utf8`),
    hash: md5(fs.readFileSync(fileName)),
  }))
  .reduce((acc,val) => { acc[val.hash] = val; return acc; }, {});

console.log(`\n`,`GUIDES`, JSON.stringify(guides,true,2));

app.get(`/guide/:hash`, (req, res) => {
  res.json(guides[req.params.hash]);
});

const puzzles = glob.sync(`${__dirname}/lessons/*/**.sh`)
  .map(fileName => ({
    type: `puzzle`,
    title: path.parse(fileName).name.replace(/^\d+ /,``),
    hash: md5(fs.readFileSync(fileName)),
    path: fileName,
  }))
  .reduce((acc,val) => { acc[val.hash] = val; return acc; }, {});

console.log(`\n`,`PUZZLES`, JSON.stringify(puzzles,true,2));

app.get(`/puzzle/:hash`, (req, res) => {
  const body = JSON.parse(require(`child_process`).execSync(`'${puzzles[req.params.hash].path}'`));
  res.json({
    ...puzzles[req.params.hash],
    body: body.map(x => ({ ...x, answer: undefined })),
    path: undefined,
  });
});

app.use((err, req, res, next) => {
  console.error(err);
  res.status(500).send({error: err});
});

app.listen(3000);
