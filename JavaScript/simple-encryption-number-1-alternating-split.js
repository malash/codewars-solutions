const encryptOnce = chs => {
  const left = [];
  const right = [];
  for (const [index, ch] of chs.entries()) {
    if (index % 2 === 1) {
      left.push(ch);
    } else {
      right.push(ch);
    }
  }
  return left.concat(right);
}

function encrypt(text, n) {
  if (typeof text !== 'string') {
    return text;
  }
  let result = Array.from(text);
  for (let i = 0; i < n; i++) {
    result = encryptOnce(result);
  }
  return result.join('');
}

const decrypttOnce = chs => {
  const mid = parseInt(chs.length / 2, 10);
  const left = chs.slice(0, mid);
  const right = chs.slice(mid, chs.length);
  const result = [];
  while (left.length || right.length) {
    if (right.length) {
      result.push(right.shift());
    }
    if (left.length) {
      result.push(left.shift());
    }
  }
  return result;
}

function decrypt(encryptedText, n) {
  if (typeof encryptedText !== 'string') {
    return encryptedText;
  }
  let result = Array.from(encryptedText);
  for (let i = 0; i < n; i++) {
    result = decrypttOnce(result);
  }
  return result.join('');
}
