#!/usr/bin/env node

const args = process.argv.slice(2);

const question = `
# hello

lorem ipsum

$$a^2 + b^2 = c^2$$
`;

console.log(
  JSON.stringify([
    {
      title: "title",
      question,
      answer: 4,
      fudge: 0.0,
    },
  ])
);
