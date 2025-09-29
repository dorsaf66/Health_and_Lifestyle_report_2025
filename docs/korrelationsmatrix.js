const data = []

function renderMatrix(matrix, keys) {
  let html = '<table border="1"><tr><th></th>';
  html += keys.map(k => `<th>${k}</th>`).join('');
  html += '</tr>';
  for (let i = 0; i < matrix.length; i++) {
    html += `<tr><th>${keys[i]}</th>`;
    for (let j = 0; j < matrix[i].length; j++) {
      html += `<td>${matrix[i][j].toFixed(2)}</td>`;
    }
    html += '</tr>';
  }
  html += '</table>';
  document.getElementById('correlation-matrix').innerHTML = html;
}

Papa.parse("data/Sleep_health_and_lifestyle_dataset.csv", {
  download: true,
  header: true,
  dynamicTyping: true,
  complete: function(results) {
    const data = results.data; // Array von Objekten
    const keys = ["Gender", "Age", "Sleep Duration", "Quality of Sleep", "Physical Activity Level", "Stress Level", "BMI Category", "Heart Rate", "Daily Steps", "Sleep Disorder"];
    // Occupation macht keinen Sinn, da es keine auf- oder absteigende Reihenfolge gibt
    const corrMatrix = correlationMatrix(data, keys);
    if(window.app && window.app.ports && window.app.ports.receiveMatrix) {
      window.app.ports.receiveMatrix.send(corrMatrix);
      console.log("Matrix an Elm gesendet");  // wird nicht angezeigt... ?
    }
    renderMatrix(corrMatrix, keys);
  }
});


function pearsonCorrelation(x, y) {
  const n = x.length;
  const avgX = x.reduce((a, b) => a + b, 0) / n;
  const avgY = y.reduce((a, b) => a + b, 0) / n;
  const numerator = x.map((xi, i) => (xi - avgX) * (y[i] - avgY)).reduce((a, b) => a + b, 0);
  const denominator = Math.sqrt(
    x.map(xi => Math.pow(xi - avgX, 2)).reduce((a, b) => a + b, 0) *
    y.map(yi => Math.pow(yi - avgY, 2)).reduce((a, b) => a + b, 0)
  );
  return numerator / denominator;
}

function correlationMatrix(data, keys) {
  const matrix = [];
  for (let i = 0; i < keys.length; i++) {
    matrix[i] = [];
    for (let j = 0; j < keys.length; j++) {
      const x = getNumericColumn(data, keys[i]);
      const y = getNumericColumn(data, keys[j]);
      matrix[i][j] = pearsonCorrelation(x, y);
    }
  }
  return matrix;
}

function labelEncode(arr) {
  const map = {};
  return arr.map(val => {
    switch (val) {
      case "Normal Weight":
      case "None":
      case "Male":
        map[val] = 0;
        break;
      case "Normal":
      case "Insomnia":
      case "Female":
        map[val] = 1;
        break;
      case "Overweight":
      case "Sleep Apnea":
        map[val] = 2;
        break;
      case "Obese":
        map[val] = 3;
        break;
    }
    return map[val];
  });
}

function getNumericColumn(data, key) {
  const col = data.map(d => d[key]);
  // überprüft ob Spalte numerisch
  if (col.every(v => typeof v === "number")) {
    return col;
  } else {
    // das betrifft: Gender, (Occupation), BMI Category, Sleep Disorder
    return labelEncode(col);
  }
}
