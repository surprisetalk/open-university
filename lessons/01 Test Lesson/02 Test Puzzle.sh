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
      id: "123456",
      title: "example puzzle",
      question,
      answer: 4,
      options: "INT",
      fudge: 0.0,
    },
  ])
);
