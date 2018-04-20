function anagrams(word, words) {
  const input = Array.from(word).sort().join('');
  return words.filter(x => {
    return input === Array.from(x).sort().join('');
  });
}
