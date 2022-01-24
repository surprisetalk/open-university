const fs = require(`fs`);
const path = require(`path`);

const randomString = () => (Math.random() + 1).toString(36).substring(7);

const app = require(`express`)();
const bodyParser = require('body-parser');

app.use(bodyParser.json());

// TODO: express.static

app.get(`/lesson*`, (req, res) => {
  const file = path.join(__dirname,`lessons`,req.params[0]);
  const lessons = fs.existsSync(file)
        ? fs.readdirSync(file)
          .filter(file => file !== `lesson.json`)
          .map(file => path.parse(file).name)
        : undefined;
  const guide = fs.existsSync(`${file}.md`) ? { title: randomString(), content: fs.readFileSync(`${file}.md`,`utf8`) } : undefined;
  const puzzle = fs.existsSync(`${file}.sh`) ? JSON.parse(require(`child_process`).execSync(`${file}.sh`)) : undefined;
  res.json({
    lessons,
    guide,
    puzzle,
  });
});

app.use((err, req, res, next) => {
  console.error(err);
  res.status(500).send({error: err});
});

app.listen(3000);
