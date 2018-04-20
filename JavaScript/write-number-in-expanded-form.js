function expandedForm(num) {
  const result = [];
  let i = 10;
  while (num > 0) {
    result.push(num % i);
    num = parseInt(num / i, 10) * i;
    i *= 10;
  }
  return result
    .filter(x => x)
    .reverse()
    .join(' + ');
}
