function alphabetPosition(text) {
  return Array
    .from(text.toLowerCase())
    .filter(c => c >= 'a' && c <= 'z')
    .map(c => c.charCodeAt(0) - 96)
    .join(' ');
}
