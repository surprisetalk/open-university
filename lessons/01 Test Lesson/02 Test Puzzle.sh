#!/usr/bin/env node

const args = process.argv.slice(2);

console.log(
  JSON.stringify([
    {
      question: `$$a^2 + b^2 = c^2$$`,
      choices: [],
      solution: Math.random() < 0.5
        ? Math.floor(Math.random()*2)
        : `${Math.floor(Math.random()*2)}`,
    },
    {
      question: `How many frogs in the hole?`,
      choices: [
        `${Math.floor(Math.random()*100)}`,
        `${Math.floor(Math.random()*100)}`,
        `${Math.floor(Math.random()*100)}`,
        `${Math.floor(Math.random()*100)}`,
      ],
      solution: Math.floor(Math.random()*4),
    },
  ])
);
