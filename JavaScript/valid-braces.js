const match = {
  ')': '(',
  ']': '[',
  '}': '{',
};

function validBraces(braces){
  const stack = [];
  for (const ch of Array.from(braces)) {
    switch(ch) {
      case '(':
      case '[':
      case '{':
        stack.push(ch);
        break;
      case ')':
      case ']':
      case '}':
        if (stack.pop() !== match[ch]) {
          return false;
        }
        break;
    }
  }
  return stack.length === 0;
}
