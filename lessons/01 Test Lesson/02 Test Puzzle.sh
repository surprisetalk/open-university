#!/usr/bin/env node

const args = process.argv.slice(2);

const question = `
# hello

lorem ipsum 

${Math.random()}

$$a^2 + b^2 = c^2$$
`;

console.log(
  JSON.stringify([
    {
      question,
      solution: "4",
      choices: [],
      // choices: ["A","B"],
      fudge: 0.0,
    },
  ])
);
