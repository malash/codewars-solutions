const aplhaMap = str => {
  const map = {};
  for (const [index, ch] of Array.from(str).entries()) {
    map[ch] = index;
  }
  return map;
}

function convert(input, source, target) {
  const sourceBase = source.length;
  const targetBase = target.length;
  const sourceMap = aplhaMap(source);
  let num = 0;
  for (const ch of Array.from(input)) {
    num = num * sourceBase + sourceMap[ch];
  }
  let result = '';
  while (num > 0) {
    result = target[num % targetBase] + result;
    num = parseInt(num / targetBase);
  }
  if (result === '') {
    return target[0];
  }
  return result;
}
