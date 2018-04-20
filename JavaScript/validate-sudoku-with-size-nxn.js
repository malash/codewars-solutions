const cmp = (a, b) => a - b;

var Sudoku = function (data) {
  return {
    isValid: function () {
      const n = data[0].length;
      const len = parseInt(Math.sqrt(n));
      if (len * len !== n) {
        return false;
      }
      for (let i = 0; i < n; i++) {
        if (data[i].length !== n) {
          return false;
        }
      }

      const isValidLine = arr => {
        const sorted = Array.from(arr).sort(cmp);
        for (let i = 0; i < n; i++) {
          if (sorted[i] !== i + 1) {
            return false;
          }
        }
        return true;
      }

      for (let i = 0; i < n; i++) {
        if (!isValidLine(data[i])) {
          return false;
        }
      }
      for (let i = 0; i < n; i++) {
        const arr = data.map(line => line[i]);
        if (!isValidLine(arr)) {
          return false;
        }
      }
      for (let i = 0; i < n; i += len) {
        for (let j = 0; j < n; j += len) {
          const arr = [];
          for (let x = 0; x < len; x++) {
            for (let y = 0; y < len; y++) {
              arr.push(data[i + x][j + y]);
            }
          }
          if (!isValidLine(arr)) {
            return false;
          }
        }
      }
      return true;
    }
  };
};
