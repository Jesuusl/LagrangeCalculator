(function (global) {
  'use strict';

  const MINUS = '−'; // Signo menos matemático real U+2212

  // ===========================================================================
  // 1. Aritmética exacta de fracciones con BigInt
  // ===========================================================================
  function gcdBigInt(a, b) {
    a = a < 0n ? -a : a;
    b = b < 0n ? -b : b;
    while (b !== 0n) {
      const t = b;
      b = a % b;
      a = t;
    }
    return a === 0n ? 1n : a;
  }

  class Frac {
    constructor(num, den = 1n) {
      let n = BigInt(num);
      let d = BigInt(den);
      if (d === 0n) {
        throw new Error('División por cero en fracción.');
      }
      if (d < 0n) {
        n = -n;
        d = -d;
      }
      if (n === 0n) {
        this.num = 0n;
        this.den = 1n;
      } else {
        const g = gcdBigInt(n, d);
        this.num = n / g;
        this.den = d / g;
      }
    }

    isZero() {
      return this.num === 0n;
    }

    isOne() {
      return this.num === 1n && this.den === 1n;
    }

    isMinusOne() {
      return this.num === -1n && this.den === 1n;
    }

    isInteger() {
      return this.den === 1n;
    }

    isNegative() {
      return this.num < 0n;
    }

    abs() {
      return new Frac(this.num < 0n ? -this.num : this.num, this.den);
    }

    neg() {
      return new Frac(-this.num, this.den);
    }

    add(other) {
      return new Frac(this.num * other.den + other.num * this.den, this.den * other.den);
    }

    sub(other) {
      return new Frac(this.num * other.den - other.num * this.den, this.den * other.den);
    }

    mul(other) {
      return new Frac(this.num * other.num, this.den * other.den);
    }

    div(other) {
      if (other.num === 0n) {
        throw new Error('División por cero.');
      }
      return new Frac(this.num * other.den, this.den * other.num);
    }

    equals(other) {
      return this.num === other.num && this.den === other.den;
    }

    toNumber() {
      return Number(this.num) / Number(this.den);
    }

    toDecimalString(maxDecimals = 4) {
      const val = this.toNumber();
      if (!isFinite(val)) return '';
      const rounded = Number(val.toFixed(maxDecimals));
      const str = rounded.toString();
      return val < 0 ? MINUS + str.replace(/^-/, '') : str;
    }

    toPlainString() {
      const sign = this.num < 0n ? MINUS : '';
      const absN = this.num < 0n ? -this.num : this.num;
      if (this.den === 1n) {
        return sign + absN.toString();
      }
      return sign + absN.toString() + '/' + this.den.toString();
    }
  }

  function parseDecimalPart(str) {
    const s = str.trim().replace(/\u2212/g, '-').replace(/,/g, '.');
    if (!/^[+-]?(\d+(\.\d+)?|\.\d+)$/.test(s)) {
      return null;
    }
    const sign = s.startsWith('-') ? -1n : 1n;
    const clean = s.replace(/^[+-]/, '');
    const parts = clean.split('.');
    if (parts.length === 1) {
      return new Frac(sign * BigInt(parts[0]), 1n);
    }
    const intPart = parts[0] || '0';
    const decPart = parts[1] || '';
    const digits = intPart + decPart;
    const den = 10n ** BigInt(decPart.length);
    return new Frac(sign * BigInt(digits), den);
  }

  function parseInputToFrac(raw) {
    if (typeof raw !== 'string') return { error: 'Valor inválido.' };
    const trimmed = raw.trim();
    if (trimmed === '') {
      return { error: 'Campo vacío.' };
    }
    if (trimmed.includes('/')) {
      const slashParts = trimmed.split('/');
      if (slashParts.length !== 2) {
        return { error: `Formato de fracción inválido: "${trimmed}".` };
      }
      const numFrac = parseDecimalPart(slashParts[0]);
      const denFrac = parseDecimalPart(slashParts[1]);
      if (!numFrac || !denFrac) {
        return { error: `Fracción no numérica: "${trimmed}".` };
      }
      if (denFrac.isZero()) {
        return { error: `El denominador no puede ser cero en "${trimmed}".` };
      }
      return { value: numFrac.div(denFrac) };
    }
    const val = parseDecimalPart(trimmed);
    if (!val) {
      return { error: `Valor no numérico: "${trimmed}".` };
    }
    return { value: val };
  }

  // ===========================================================================
  // 2. Operaciones con polinomios de coeficientes fraccionarios
  // ===========================================================================
  function polyTrim(p) {
    const res = p.slice();
    while (res.length > 1 && res[res.length - 1].isZero()) {
      res.pop();
    }
    return res.length === 0 ? [new Frac(0n)] : res;
  }

  function polyAdd(p, q) {
    const maxLen = Math.max(p.length, q.length);
    const res = [];
    for (let i = 0; i < maxLen; i++) {
      const a = i < p.length ? p[i] : new Frac(0n);
      const b = i < q.length ? q[i] : new Frac(0n);
      res.push(a.add(b));
    }
    return polyTrim(res);
  }

  function polyScale(p, scalarFrac) {
    return polyTrim(p.map(c => c.mul(scalarFrac)));
  }

  function polyMul(p, q) {
    const res = Array.from({ length: p.length + q.length - 1 }, () => new Frac(0n));
    for (let i = 0; i < p.length; i++) {
      for (let j = 0; j < q.length; j++) {
        res[i + j] = res[i + j].add(p[i].mul(q[j]));
      }
    }
    return polyTrim(res);
  }

  function polyEval(p, xFrac) {
    let acc = new Frac(0n);
    for (let i = p.length - 1; i >= 0; i--) {
      acc = acc.mul(xFrac).add(p[i]);
    }
    return acc;
  }

  function polyEvalNum(p, xNum) {
    let acc = 0;
    for (let i = p.length - 1; i >= 0; i--) {
      acc = acc * xNum + p[i].toNumber();
    }
    return acc;
  }

  function computeLagrangePolynomial(workNodes) {
    const k = workNodes.length;
    const basisPolys = [];
    for (let i = 0; i < k; i++) {
      const xi = workNodes[i].x;
      let monicPoly = [new Frac(1n)];
      let D = new Frac(1n);
      for (let j = 0; j < k; j++) {
        if (j === i) continue;
        const xj = workNodes[j].x;
        monicPoly = polyMul(monicPoly, [xj.neg(), new Frac(1n)]);
        D = D.mul(xi.sub(xj));
      }
      const invD = new Frac(1n).div(D);
      basisPolys.push(polyScale(monicPoly, invD));
    }
    const scaledPolys = workNodes.map((pt, i) => polyScale(basisPolys[i], pt.y));
    let finalPoly = [new Frac(0n)];
    for (const sp of scaledPolys) {
      finalPoly = polyAdd(finalPoly, sp);
    }
    return { basisPolys, scaledPolys, finalPoly };
  }

  // ===========================================================================
  // 3. Maquetación HTML de fracciones y polinomios
  // ===========================================================================
  function renderVerticalFrac(absNum, den) {
    return (
      '<span class="frac">' +
        '<span class="frac-num">' + absNum.toString() + '</span>' +
        '<span class="frac-bar">/</span>' +
        '<span class="frac-den">' + den.toString() + '</span>' +
      '</span>'
    );
  }

  function renderFracHTML(frac, opts = {}) {
    const { parensIfNeg = false, parensIfFrac = false, forcePlus = false } = opts;
    const isNeg = frac.isNegative();
    const absN = isNeg ? -frac.num : frac.num;
    const sign = isNeg ? MINUS : (forcePlus && !frac.isZero() ? '+' : '');

    let core;
    if (frac.isInteger()) {
      core = sign + absN.toString();
      if (parensIfNeg && isNeg) {
        return '(' + core + ')';
      }
      return core;
    } else {
      core = sign + renderVerticalFrac(absN, frac.den);
      if ((parensIfNeg && isNeg) || parensIfFrac) {
        return '(' + core + ')';
      }
      return core;
    }
  }

  function renderTermWithSignHTML(coeff, degree, isFirstTerm) {
    const isNeg = coeff.isNegative();
    const absC = coeff.abs();
    const signPrefix = isFirstTerm
      ? (isNeg ? MINUS : '')
      : (isNeg ? ' ' + MINUS + ' ' : ' + ');

    let varPart = '';
    if (degree === 1) {
      varPart = 'x';
    } else if (degree > 1) {
      varPart = 'x<sup>' + degree + '</sup>';
    }

    let coeffPart = '';
    if (degree === 0) {
      coeffPart = renderFracHTML(absC);
    } else if (absC.isOne()) {
      coeffPart = '';
    } else {
      coeffPart = renderFracHTML(absC);
    }

    return signPrefix + coeffPart + varPart;
  }

  function renderPolyHTML(poly) {
    const terms = [];
    for (let k = poly.length - 1; k >= 0; k--) {
      if (!poly[k].isZero()) {
        terms.push(renderTermWithSignHTML(poly[k], k, terms.length === 0));
      }
    }
    return terms.length > 0 ? terms.join('') : '0';
  }

  function renderPolyEvaluationSubstitutionHTML(poly, xVal) {
    const xParen = `(<span class="sub-x-highlight">${renderFracHTML(xVal)}</span>)`;
    const evalTerms = [];
    for (let deg = poly.length - 1; deg >= 0; deg--) {
      const c = poly[deg];
      if (c.isZero()) continue;
      const signPrefix = evalTerms.length === 0
        ? (c.isNegative() ? MINUS : '')
        : (c.isNegative() ? ' ' + MINUS + ' ' : ' + ');
      const absC = c.abs();

      if (deg === 0) {
        evalTerms.push(signPrefix + renderFracHTML(absC));
      } else {
        const coeffStr = absC.isOne() ? '' : renderFracHTML(absC);
        const powStr = deg === 1 ? xParen : `${xParen}<sup>${deg}</sup>`;
        evalTerms.push(signPrefix + coeffStr + powStr);
      }
    }
    return evalTerms.length > 0 ? evalTerms.join('') : '0';
  }

  function renderNumeratorFactorHTML(xj, totalFactors, highlightReplaced = false) {
    if (xj.isZero()) {
      return highlightReplaced ? '<span class="sub-x-highlight">x</span>' : 'x';
    }
    if (xj.isNegative()) {
      const numStr = highlightReplaced
        ? `<span class="sub-x-highlight">${renderFracHTML(xj.abs())}</span>`
        : renderFracHTML(xj.abs());
      const inner = 'x + ' + numStr;
      return totalFactors === 1 ? '(' + inner + ')' : '(' + inner + ')';
    }
    const numStr = highlightReplaced
      ? `<span class="sub-x-highlight">${renderFracHTML(xj)}</span>`
      : renderFracHTML(xj);
    const inner = 'x ' + MINUS + ' ' + numStr;
    return '(' + inner + ')';
  }

  function renderNewtonFactorHTML(xj, highlightReplaced = false) {
    if (xj.isZero()) {
      const zeroStr = highlightReplaced ? '<span class="sub-x-highlight">0</span>' : '0';
      return '(x ' + MINUS + ' ' + zeroStr + ')';
    }
    if (xj.isNegative()) {
      const numStr = highlightReplaced
        ? `<span class="sub-x-highlight">${renderFracHTML(xj.abs())}</span>`
        : renderFracHTML(xj.abs());
      return '(x + ' + numStr + ')';
    }
    const numStr = highlightReplaced
      ? `<span class="sub-x-highlight">${renderFracHTML(xj)}</span>`
      : renderFracHTML(xj);
    return '(x ' + MINUS + ' ' + numStr + ')';
  }

  function renderUnsimplifiedTwoFactorsHTML(x1Node, x2Node) {
    const c1 = x1Node.neg();
    const c2 = x2Node.neg();
    const c1c2 = c1.mul(c2);

    const rawTerms = [
      { coeff: new Frac(1n), deg: 2 },
      { coeff: c2, deg: 1 },
      { coeff: c1, deg: 1 },
      { coeff: c1c2, deg: 0 }
    ];

    const rendered = [];
    for (const item of rawTerms) {
      if (!item.coeff.isZero()) {
        rendered.push(renderTermWithSignHTML(item.coeff, item.deg, rendered.length === 0));
      }
    }
    return rendered.join('');
  }

  // ===========================================================================
  // 3b. Generadores de explicación desmenuzada paso a paso con botón (?)
  // ===========================================================================
  function renderWhyBoxHTML(label, innerHTML) {
    return `
      <details class="why-box">
        <summary class="why-btn" title="Haz clic para ver el porqué paso a paso">
          <span class="why-icon" aria-hidden="true">?</span>
          <span class="why-btn-text">${label}</span>
        </summary>
        <div class="why-panel">
          ${innerHTML}
        </div>
      </details>
    `;
  }

  // Explica paso a paso la sustitución de factores (x - x_j), la multiplicación de paréntesis (todos contra todos + cancelaciones) y la multiplicación por el coeficiente externo
  function buildFactorMultiplicationBreakdownHTML(factorNodes, factorNodeIndices, outerCoeff, outerCoeffLabel, useNewtonZero = false) {
    const items = [];

    // 1. Por qué quedó así la sustitución de los factores
    const subBullets = factorNodes.map((xj, idx) => {
      const nodeIdx = factorNodeIndices ? factorNodeIndices[idx] : idx;
      const fRendered = useNewtonZero ? renderNewtonFactorHTML(xj) : renderNumeratorFactorHTML(xj, factorNodes.length);
      if (xj.isNegative()) {
        return `<li>En <span class="math-inline">(x ${MINUS} x<sub>${nodeIdx}</sub>)</span>, como <span class="math-inline">x<sub>${nodeIdx}</sub> = ${renderFracHTML(xj)}</span> es negativo, tenemos doble signo menos: <span class="math-inline">(x ${MINUS} (${renderFracHTML(xj)})) = <strong>${fRendered}</strong></span> (por ley de signos: menos por menos da más).</li>`;
      }
      if (xj.isZero()) {
        return useNewtonZero
          ? `<li>En <span class="math-inline">(x ${MINUS} x<sub>${nodeIdx}</sub>)</span>, reemplazamos <span class="math-inline">x<sub>${nodeIdx}</sub> = 0</span> y queda explícitamente <span class="math-inline"><strong>(x ${MINUS} 0)</strong></span> (que al operar equivale a <span class="math-inline">x</span>).</li>`
          : `<li>En <span class="math-inline">(x ${MINUS} x<sub>${nodeIdx}</sub>)</span>, como <span class="math-inline">x<sub>${nodeIdx}</sub> = 0</span>, al restar cero queda simplemente <span class="math-inline">x ${MINUS} 0 = <strong>x</strong></span>.</li>`;
      }
      return `<li>En <span class="math-inline">(x ${MINUS} x<sub>${nodeIdx}</sub>)</span>, reemplazamos directamente <span class="math-inline">x<sub>${nodeIdx}</sub> = ${renderFracHTML(xj)}</span> y queda <span class="math-inline"><strong>${fRendered}</strong></span>.</li>`;
    }).join('');

    const coeffNote = outerCoeff.isNegative()
      ? `El coeficiente <span class="math-inline">${outerCoeffLabel} = ${renderFracHTML(outerCoeff)}</span> se encierra entre paréntesis <span class="math-inline">(${renderFracHTML(outerCoeff)})</span> porque es negativo.`
      : `El coeficiente <span class="math-inline">${outerCoeffLabel} = ${renderFracHTML(outerCoeff)}</span> es positivo, así que se escribe sin paréntesis delante de los factores.`;

    items.push(`
      <li>
        <strong>1. ¿Por qué queda así al reemplazar los datos?</strong>
        <div>${coeffNote}</div>
        <ul class="why-sublist">${subBullets}</ul>
      </li>
    `);

    // 2. Multiplicación progresiva de los paréntesis (si hay 2 o más factores)
    let currentPoly = [factorNodes[0].neg(), new Frac(1n)]; // (x - x_0)
    if (factorNodes.length >= 2) {
      for (let m = 1; m < factorNodes.length; m++) {
        const nextRoot = factorNodes[m];
        const cNext = nextRoot.neg(); // término constante de (x - nextRoot), es decir, +cNext
        const prevPolyHTML = m === 1
          ? (useNewtonZero ? renderNewtonFactorHTML(factorNodes[0]) : renderNumeratorFactorHTML(factorNodes[0], 2))
          : `(${renderPolyHTML(currentPoly)})`;
        const nextFactorHTML = useNewtonZero
          ? renderNewtonFactorHTML(nextRoot)
          : renderNumeratorFactorHTML(nextRoot, factorNodes.length);

        // Multiplicar término a término currentPoly por (x + cNext)
        const rawProducts = [];
        const distribBullets = [];
        for (let deg = currentPoly.length - 1; deg >= 0; deg--) {
          const coeff = currentPoly[deg];
          if (coeff.isZero()) continue;
          const termHTML = renderTermWithSignHTML(coeff, deg, true);

          // Por x
          const prodXDeg = deg + 1;
          const prodXCoeff = coeff;
          rawProducts.push({ coeff: prodXCoeff, deg: prodXDeg });
          distribBullets.push(`<li>Multiplicamos <span class="math-inline">${termHTML} · x = <strong>${renderTermWithSignHTML(prodXCoeff, prodXDeg, true)}</strong></span></li>`);

          // Por cNext
          const prodCDeg = deg;
          const prodCCoeff = coeff.mul(cNext);
          const cNextStr = cNext.isNegative()
            ? `(${renderFracHTML(cNext)})`
            : (nextRoot.isZero() ? `(${MINUS}0)` : renderFracHTML(cNext, { forcePlus: true }));
          if (!prodCCoeff.isZero()) {
            rawProducts.push({ coeff: prodCCoeff, deg: prodCDeg });
            distribBullets.push(`<li>Multiplicamos <span class="math-inline">${termHTML} · ${cNextStr} = <strong>${renderTermWithSignHTML(prodCCoeff, prodCDeg, true)}</strong></span></li>`);
          } else {
            distribBullets.push(`<li>Multiplicamos <span class="math-inline">${termHTML} · ${cNextStr} = <strong>0</strong></span> (se anula por multiplicar por cero)</li>`);
          }
        }

        const unsimplifiedJoined = rawProducts.length > 0
          ? rawProducts.map((p, idx) => renderTermWithSignHTML(p.coeff, p.deg, idx === 0)).join('')
          : '0';

        const nextPoly = polyMul(currentPoly, [cNext, new Frac(1n)]);

        // Revisar qué grados tuvieron términos semejantes que se sumaron o se cancelaron
        const combineNotes = [];
        for (let d = nextPoly.length - 1; d >= 0; d--) {
          const matching = rawProducts.filter(p => p.deg === d);
          if (matching.length > 1) {
            const sumStr = matching.map((p, idx) => renderTermWithSignHTML(p.coeff, d, idx === 0)).join('');
            const resCoeff = nextPoly[d];
            if (resCoeff.isZero()) {
              combineNotes.push(`<li>En grado ${d} (<span class="math-inline">${d === 1 ? 'x' : `x<sup>${d}</sup>`}</span>): <span class="math-inline">${sumStr} = <strong>0</strong></span> → <strong>¡se cancelan entre sí por tener signos opuestos y desaparecen!</strong></li>`);
            } else {
              combineNotes.push(`<li>Agrupamos términos con <span class="math-inline">${d === 1 ? 'x' : `x<sup>${d}</sup>`}</span>: <span class="math-inline">${sumStr} = <strong>${renderTermWithSignHTML(resCoeff, d, true)}</strong></span>.</li>`);
            }
          }
        }

        const remainingWait = factorNodes.slice(m + 1)
          .map(r => useNewtonZero ? renderNewtonFactorHTML(r) : renderNumeratorFactorHTML(r, factorNodes.length))
          .join('');
        const waitNote = remainingWait
          ? ` (mientras que <span class="math-inline">${remainingWait}</span> se baja igual esperando el siguiente paso)`
          : '';

        items.push(`
          <li>
            <strong>${m + 1}. Multiplicamos <span class="math-inline">${prevPolyHTML}</span> por <span class="math-inline">${nextFactorHTML}</span> (propiedad distributiva, todos contra todos):</strong>
            <ul class="why-sublist">
              ${distribBullets.join('')}
            </ul>
            <div>Al juntar esos resultados tenemos: <span class="math-inline"><strong>${unsimplifiedJoined}</strong></span>.</div>
            ${combineNotes.length > 0 ? `<ul class="why-sublist">${combineNotes.join('')}</ul>` : ''}
            <div>Por eso este producto simplificado da <span class="math-inline"><strong>(${renderPolyHTML(nextPoly)})</strong></span>${waitNote}.</div>
          </li>
        `);

        currentPoly = nextPoly;
      }
    }

    // 3. Multiplicación final por el coeficiente de afuera
    const finalScaledPoly = polyScale(currentPoly, outerCoeff);
    const scaleStepNum = factorNodes.length + 1;
    if (outerCoeff.isOne()) {
      items.push(`
        <li>
          <strong>${scaleStepNum}. Multiplicamos por <span class="math-inline">${outerCoeffLabel} = 1</span> de afuera:</strong>
          Como multiplicar por <span class="math-inline">1</span> deja igual a cualquier expresión, quitamos el paréntesis y queda directamente <span class="math-inline"><strong>${renderPolyHTML(finalScaledPoly)}</strong></span>.
        </li>
      `);
    } else if (outerCoeff.isZero()) {
      items.push(`
        <li>
          <strong>${scaleStepNum}. Multiplicamos por <span class="math-inline">${outerCoeffLabel} = 0</span>:</strong>
          Todo número multiplicado por <span class="math-inline">0</span> da <span class="math-inline"><strong>0</strong></span>.
        </li>
      `);
    } else {
      const scaleBullets = [];
      for (let d = currentPoly.length - 1; d >= 0; d--) {
        const c = currentPoly[d];
        if (c.isZero()) continue;
        const prod = c.mul(outerCoeff);
        scaleBullets.push(`
          <li><span class="math-inline">${renderFracHTML(outerCoeff, { parensIfNeg: true })} · (${renderTermWithSignHTML(c, d, true)}) = <strong>${renderTermWithSignHTML(prod, d, true)}</strong></span></li>
        `);
      }
      items.push(`
        <li>
          <strong>${scaleStepNum}. Distribuimos el coeficiente <span class="math-inline">${outerCoeffLabel} = ${renderFracHTML(outerCoeff)}</span> por cada término del paréntesis <span class="math-inline">(${renderPolyHTML(currentPoly)})</span>:</strong>
          <ul class="why-sublist">${scaleBullets.join('')}</ul>
          <div>Obteniendo el resultado final: <span class="math-inline"><strong>${renderPolyHTML(finalScaledPoly)}</strong></span>.</div>
        </li>
      `);
    }

    return `<ol class="why-step-list" style="list-style: none; padding-left: 0;">${items.join('')}</ol>`;
  }

  // Explica paso a paso de dónde sale cada coeficiente al agrupar por grados y cómo se suman
  function buildDegreeGroupingBreakdownHTML(deg, polyList, sourceLabels, totalAtDeg) {
    const varLabel = deg === 0 ? 'término independiente (sin x)' : (deg === 1 ? 'x' : `x<sup>${deg}</sup>`);
    const varShort = deg === 0 ? '' : (deg === 1 ? 'x' : `x<sup>${deg}</sup>`);
    const fromBullets = [];
    const nonZeroItems = [];

    for (let i = 0; i < polyList.length; i++) {
      const p = polyList[i];
      const c = deg < p.length ? p[deg] : new Frac(0n);
      if (!c.isZero()) {
        nonZeroItems.push({ label: sourceLabels[i], coeff: c });
        fromBullets.push(`<li>De <strong>${sourceLabels[i]}</strong> (<span class="math-inline">${renderPolyHTML(p)}</span>) tomamos <span class="math-inline"><strong>${renderTermWithSignHTML(c, deg, true)}</strong></span> (coeficiente <span class="math-inline"><strong>${renderFracHTML(c)}</strong></span>).</li>`);
      } else {
        fromBullets.push(`<li>En <strong>${sourceLabels[i]}</strong> (<span class="math-inline">${renderPolyHTML(p)}</span>) no aparece <span class="math-inline">${varLabel}</span> (aporta <span class="math-inline">0</span>).</li>`);
      }
    }

    let sumExplain = '';
    if (nonZeroItems.length === 0) {
      sumExplain = `Como ningún término tiene <span class="math-inline">${varLabel}</span>, la suma es <span class="math-inline"><strong>0</strong></span>.`;
    } else if (nonZeroItems.length === 1) {
      sumExplain = `Como solo hay un término con <span class="math-inline">${varLabel}</span>, no hay nada más que sumar y pasa directo como <span class="math-inline"><strong>${renderTermWithSignHTML(totalAtDeg, deg, true)}</strong></span>.`;
    } else {
      const runningSteps = [];
      let acc = nonZeroItems[0].coeff;
      for (let k = 1; k < nonZeroItems.length; k++) {
        const nextC = nonZeroItems[k].coeff;
        const newAcc = acc.add(nextC);
        const opSign = nextC.isNegative() ? `${MINUS} ${renderFracHTML(nextC.abs())}` : `+ ${renderFracHTML(nextC)}`;
        runningSteps.push(`<li>Operamos <span class="math-inline">${renderFracHTML(acc)} ${opSign} = <strong>${renderFracHTML(newAcc)}</strong></span>${newAcc.isZero() ? ' (se cancelan y da 0)' : ''}.</li>`);
        acc = newAcc;
      }
      sumExplain = `
        <div><strong>Sumamos/restamos los coeficientes paso a paso:</strong></div>
        <ul class="why-sublist">${runningSteps.join('')}</ul>
        <div>Resultado final para este grado: <span class="math-inline"><strong>${renderTermWithSignHTML(totalAtDeg, deg, true)}</strong></span>${totalAtDeg.isZero() ? ` (como dio 0, no se escribe <span class="math-inline">${varShort}</span> en el polinomio final)` : ''}.</div>
      `;
    }

    return `
      <div><strong>¿De dónde salieron los números de este grado?</strong> Revisamos cada término desarrollado:</div>
      <ul class="why-sublist">${fromBullets.join('')}</ul>
      ${sumExplain}
    `;
  }

  // Explica paso a paso cómo se evalúa P(x) en un valor xVal (potencias, productos y suma)
  function buildPolyEvaluationBreakdownHTML(poly, xVal) {
    const termSteps = [];
    const evaluatedParts = [];

    for (let deg = poly.length - 1; deg >= 0; deg--) {
      const c = poly[deg];
      if (c.isZero()) continue;

      if (deg === 0) {
        evaluatedParts.push(c);
        termSteps.push(`<li><strong>Término independiente:</strong> queda igual, <span class="math-inline"><strong>${renderFracHTML(c)}</strong></span>.</li>`);
      } else if (deg === 1) {
        const prod = c.mul(xVal);
        evaluatedParts.push(prod);
        termSteps.push(`<li><strong>Término de grado 1 (<span class="math-inline">${renderTermWithSignHTML(c, 1, true)}</span>):</strong> reemplazamos <span class="math-inline">x = ${renderFracHTML(xVal)}</span> → <span class="math-inline">${renderFracHTML(c, { parensIfNeg: true })} · (${renderFracHTML(xVal)}) = <strong>${renderFracHTML(prod)}</strong></span>.</li>`);
      } else {
        let powVal = new Frac(1n);
        for (let p = 0; p < deg; p++) powVal = powVal.mul(xVal);
        const prod = c.mul(powVal);
        evaluatedParts.push(prod);
        termSteps.push(`<li><strong>Término de grado ${deg} (<span class="math-inline">${renderTermWithSignHTML(c, deg, true)}</span>):</strong> primero elevamos <span class="math-inline">(${renderFracHTML(xVal)})<sup>${deg}</sup> = ${renderFracHTML(powVal)}</span>, y luego multiplicamos por su coeficiente: <span class="math-inline">${renderFracHTML(c, { parensIfNeg: true })} · (${renderFracHTML(powVal)}) = <strong>${renderFracHTML(prod)}</strong></span>.</li>`);
      }
    }

    const totalVal = polyEval(poly, xVal);
    const sumLine = evaluatedParts.map((v, i) => renderTermWithSignHTML(v, 0, i === 0)).join('');

    return `
      <div><strong>1. Calculamos cada término reemplazando <span class="math-inline">x = ${renderFracHTML(xVal)}</span>:</strong></div>
      <ul class="why-sublist">${termSteps.join('')}</ul>
      <div><strong>2. Sumamos todos los resultados obtenidos:</strong> <span class="math-inline">${sumLine} = <strong>${renderFracHTML(totalVal)}</strong></span>.</div>
    `;
  }

  // ===========================================================================
  // 4. Generador de gráficas SVG (origen incluido, paso agradable)
  // ===========================================================================
  function niceStep(span, targetTicks = 6) {
    if (span <= 0 || !isFinite(span)) return 1;
    const raw = span / targetTicks;
    const exp = Math.floor(Math.log10(raw));
    const base = Math.pow(10, exp);
    const frac = raw / base;
    let nice;
    if (frac <= 1) nice = 1;
    else if (frac <= 2) nice = 2;
    else if (frac <= 5) nice = 5;
    else nice = 10;
    return nice * base;
  }

  function formatTickNumber(val) {
    const clean = Math.abs(val) < 1e-10 ? 0 : Number(val.toPrecision(10));
    if (clean < 0) {
      return MINUS + Math.abs(clean).toString();
    }
    return clean.toString();
  }

  function buildCartesianSVG(planeNodes, polyOrNull, ariaLabel, evalPoint = null) {
    const xs = planeNodes.map(pt => pt.x.toNumber());
    const ys = planeNodes.map(pt => pt.y.toNumber());
    if (evalPoint && isFinite(evalPoint.x.toNumber()) && isFinite(evalPoint.y.toNumber())) {
      xs.push(evalPoint.x.toNumber());
      ys.push(evalPoint.y.toNumber());
    }

    let minXData = Math.min(0, ...xs);
    let maxXData = Math.max(0, ...xs);
    let minYData = Math.min(0, ...ys);
    let maxYData = Math.max(0, ...ys);

    if (polyOrNull) {
      const nodeMinX = Math.min(...xs);
      const nodeMaxX = Math.max(...xs);
      const nodeYSpan = Math.max(2, maxYData - minYData);
      const clampLimit = Math.max(nodeYSpan * 1.6, 6);
      for (let s = 0; s <= 60; s++) {
        const tx = nodeMinX + (nodeMaxX - nodeMinX) * (s / 60);
        const ty = polyEvalNum(polyOrNull, tx);
        if (isFinite(ty) && Math.abs(ty) <= clampLimit) {
          minYData = Math.min(minYData, ty);
          maxYData = Math.max(maxYData, ty);
        }
      }
    }

    const rawSpanX = Math.max(2, maxXData - minXData);
    const rawSpanY = Math.max(2, maxYData - minYData);

    const stepX = niceStep(rawSpanX * 1.35, 7);
    const stepY = niceStep(rawSpanY * 1.35, 6);

    const xMin = Math.floor((minXData - stepX * 0.65) / stepX) * stepX;
    const xMax = Math.ceil((maxXData + stepX * 0.85) / stepX) * stepX;
    const yMin = Math.floor((minYData - stepY * 0.65) / stepY) * stepY;
    const yMax = Math.ceil((maxYData + stepY * 0.85) / stepY) * stepY;

    const width = 680;
    const height = 380;
    const padLeft = 48;
    const padRight = 28;
    const padTop = 24;
    const padBottom = 38;
    const plotW = width - padLeft - padRight;
    const plotH = height - padTop - padBottom;

    function toSvgX(x) {
      return padLeft + ((x - xMin) / (xMax - xMin)) * plotW;
    }

    function toSvgY(y) {
      return padTop + ((yMax - y) / (yMax - yMin)) * plotH;
    }

    const originX = toSvgX(0);
    const originY = toSvgY(0);

    let gridLines = '';
    let tickLabels = '';

    const numStepsX = Math.round((xMax - xMin) / stepX);
    for (let i = 0; i <= numStepsX; i++) {
      const val = xMin + i * stepX;
      const sx = toSvgX(val);
      gridLines += `<line x1="${sx.toFixed(2)}" y1="${padTop}" x2="${sx.toFixed(2)}" y2="${height - padBottom}" stroke="var(--svg-grid)" stroke-width="1" />`;
      gridLines += `<line x1="${sx.toFixed(2)}" y1="${(originY - 4).toFixed(2)}" x2="${sx.toFixed(2)}" y2="${(originY + 4).toFixed(2)}" stroke="var(--svg-axis)" stroke-width="1.4" />`;
      if (Math.abs(val) > 1e-9) {
        const labelY = Math.min(height - 10, Math.max(padTop + 14, originY + 18));
        tickLabels += `<text x="${sx.toFixed(2)}" y="${labelY.toFixed(2)}" text-anchor="middle" fill="var(--ink-secondary)" font-size="12">${formatTickNumber(val)}</text>`;
      }
    }

    const numStepsY = Math.round((yMax - yMin) / stepY);
    for (let j = 0; j <= numStepsY; j++) {
      const val = yMin + j * stepY;
      const sy = toSvgY(val);
      gridLines += `<line x1="${padLeft}" y1="${sy.toFixed(2)}" x2="${width - padRight}" y2="${sy.toFixed(2)}" stroke="var(--svg-grid)" stroke-width="1" />`;
      gridLines += `<line x1="${(originX - 4).toFixed(2)}" y1="${sy.toFixed(2)}" x2="${(originX + 4).toFixed(2)}" y2="${sy.toFixed(2)}" stroke="var(--svg-axis)" stroke-width="1.4" />`;
      if (Math.abs(val) > 1e-9) {
        const labelX = Math.max(18, Math.min(width - padRight - 8, originX - 8));
        tickLabels += `<text x="${labelX.toFixed(2)}" y="${(sy + 4).toFixed(2)}" text-anchor="end" fill="var(--ink-secondary)" font-size="12">${formatTickNumber(val)}</text>`;
      }
    }

    tickLabels += `<text x="${(originX - 7).toFixed(2)}" y="${(originY + 16).toFixed(2)}" text-anchor="end" fill="var(--ink-secondary)" font-size="11">0</text>`;

    const axesHTML = `
      <line x1="${padLeft}" y1="${originY.toFixed(2)}" x2="${width - padRight}" y2="${originY.toFixed(2)}" stroke="var(--svg-axis)" stroke-width="2" />
      <line x1="${originX.toFixed(2)}" y1="${padTop}" x2="${originX.toFixed(2)}" y2="${height - padBottom}" stroke="var(--svg-axis)" stroke-width="2" />
      <text x="${width - padRight - 4}" y="${(originY - 8).toFixed(2)}" text-anchor="end" fill="var(--ink-primary)" font-style="italic" font-weight="600" font-size="14">x</text>
      <text x="${(originX + 10).toFixed(2)}" y="${padTop + 12}" text-anchor="start" fill="var(--ink-primary)" font-style="italic" font-weight="600" font-size="14">y</text>
    `;

    let curveHTML = '';
    if (polyOrNull) {
      const SAMPLES = 300;
      const pathParts = [];
      let prevInside = false;
      let prevX = 0;
      let prevY = 0;

      function clampBoundaryY(yVal) {
        return yVal > yMax ? yMax : yMin;
      }

      for (let i = 0; i <= SAMPLES; i++) {
        const xVal = xMin + ((xMax - xMin) * i) / SAMPLES;
        const yVal = polyEvalNum(polyOrNull, xVal);
        const isInside = isFinite(yVal) && yVal >= yMin && yVal <= yMax;

        if (isInside) {
          if (!prevInside && i > 0 && isFinite(prevY)) {
            const boundY = clampBoundaryY(prevY);
            const t = (boundY - prevY) / (yVal - prevY);
            const ix = prevX + t * (xVal - prevX);
            pathParts.push(`M ${toSvgX(ix).toFixed(2)} ${toSvgY(boundY).toFixed(2)}`);
            pathParts.push(`L ${toSvgX(xVal).toFixed(2)} ${toSvgY(yVal).toFixed(2)}`);
          } else if (!prevInside) {
            pathParts.push(`M ${toSvgX(xVal).toFixed(2)} ${toSvgY(yVal).toFixed(2)}`);
          } else {
            pathParts.push(`L ${toSvgX(xVal).toFixed(2)} ${toSvgY(yVal).toFixed(2)}`);
          }
        } else if (prevInside && isFinite(yVal)) {
          const boundY = clampBoundaryY(yVal);
          const t = (boundY - prevY) / (yVal - prevY);
          const ix = prevX + t * (xVal - prevX);
          pathParts.push(`L ${toSvgX(ix).toFixed(2)} ${toSvgY(boundY).toFixed(2)}`);
        }

        prevInside = isInside;
        prevX = xVal;
        prevY = yVal;
      }

      if (pathParts.length > 0) {
        curveHTML = `<path d="${pathParts.join(' ')}" fill="none" stroke="var(--svg-curve)" stroke-width="2.6" stroke-linecap="round" stroke-linejoin="round" />`;
      }
    }

    let pointsHTML = '';
    let hasInactive = false;
    planeNodes.forEach((pt) => {
      const isWork = pt.active !== false;
      if (!isWork) hasInactive = true;
      const sx = toSvgX(pt.x.toNumber());
      const sy = toSvgY(pt.y.toNumber());
      const labelText = `(${pt.x.toPlainString()}, ${pt.y.toPlainString()})`;

      const offsetX = sx > width - padRight - 95 ? -10 : 10;
      const anchor = sx > width - padRight - 95 ? 'end' : 'start';
      const offsetY = sy < padTop + 24 ? 18 : -10;

      if (isWork) {
        pointsHTML += `
          <g>
            <circle cx="${sx.toFixed(2)}" cy="${sy.toFixed(2)}" r="5.5" fill="var(--svg-point)" stroke="var(--bg-card)" stroke-width="2" />
            <text x="${(sx + offsetX).toFixed(2)}" y="${(sy + offsetY).toFixed(2)}" text-anchor="${anchor}" fill="var(--ink-primary)" font-size="13" font-weight="600" stroke="var(--svg-label-bg)" stroke-width="4" paint-order="stroke">${labelText}</text>
          </g>
        `;
      } else {
        pointsHTML += `
          <g>
            <circle cx="${sx.toFixed(2)}" cy="${sy.toFixed(2)}" r="5" fill="var(--bg-paper)" stroke="var(--svg-point-inactive)" stroke-width="2" stroke-dasharray="2.5 2" />
            <text x="${(sx + offsetX).toFixed(2)}" y="${(sy + offsetY).toFixed(2)}" text-anchor="${anchor}" fill="var(--ink-muted)" font-size="12" stroke="var(--svg-label-bg)" stroke-width="4" paint-order="stroke">${labelText}</text>
          </g>
        `;
      }
    });

    if (evalPoint) {
      const ex = toSvgX(evalPoint.x.toNumber());
      const ey = toSvgY(evalPoint.y.toNumber());
      if (ey >= padTop && ey <= height - padBottom) {
        const eLabel = `P(${evalPoint.x.toPlainString()}) = ${evalPoint.y.toPlainString()}`;
        const offsetX = ex > width - padRight - 125 ? -10 : 10;
        const anchor = ex > width - padRight - 125 ? 'end' : 'start';
        const offsetY = ey < padTop + 26 ? 20 : -12;
        pointsHTML += `
          <g>
            <circle cx="${ex.toFixed(2)}" cy="${ey.toFixed(2)}" r="6.5" fill="var(--svg-point-eval)" stroke="var(--bg-card)" stroke-width="2" />
            <text x="${(ex + offsetX).toFixed(2)}" y="${(ey + offsetY).toFixed(2)}" text-anchor="${anchor}" fill="var(--check-green)" font-size="13" font-weight="700" stroke="var(--svg-label-bg)" stroke-width="4" paint-order="stroke">${eLabel}</text>
          </g>
        `;
      }
    }

    let legendHTML = '';
    if (hasInactive || evalPoint || polyOrNull) {
      legendHTML = `
        <div class="plot-legend">
          <span class="legend-item"><span class="legend-dot legend-dot-active"></span> Nodos de trabajo (${planeNodes.filter(p => p.active !== false).length})</span>
          ${hasInactive ? `<span class="legend-item"><span class="legend-dot legend-dot-inactive"></span> Otros nodos del plano (${planeNodes.filter(p => p.active === false).length})</span>` : ''}
          ${evalPoint ? `<span class="legend-item"><span class="legend-dot legend-dot-eval"></span> Punto evaluado (${evalPoint.x.toPlainString()}, ${evalPoint.y.toPlainString()})</span>` : ''}
          ${polyOrNull ? `<span class="legend-item plot-interactive-hint">💡 Mueve el cursor sobre la gráfica para explorar (x, P(x))</span>` : ''}
        </div>
      `;
    }

    const polyDataAttrs = polyOrNull
      ? `data-poly-coeffs="${polyOrNull.map(c => c.toNumber()).join(',')}" data-xmin="${xMin}" data-xmax="${xMax}" data-ymin="${yMin}" data-ymax="${yMax}" data-pad-left="${padLeft}" data-pad-right="${padRight}" data-pad-top="${padTop}" data-pad-bottom="${padBottom}" data-width="${width}" data-height="${height}"`
      : '';

    const interactiveOverlayHTML = polyOrNull
      ? `
        <g class="svg-hover-group" style="display: none; pointer-events: none;">
          <line class="svg-hover-vline" x1="0" y1="${padTop}" x2="0" y2="${height - padBottom}" stroke="var(--accent-blue)" stroke-width="1.3" stroke-dasharray="4 3" opacity="0.8" />
          <line class="svg-hover-hline" x1="${padLeft}" y1="0" x2="${width - padRight}" y2="0" stroke="var(--accent-blue)" stroke-width="1.3" stroke-dasharray="4 3" opacity="0.8" />
          <circle class="svg-hover-dot" cx="0" cy="0" r="6" fill="var(--accent-blue)" stroke="var(--bg-card)" stroke-width="2.2" />
          <g class="svg-hover-badge">
            <rect class="svg-hover-badge-bg" x="0" y="0" width="148" height="26" rx="5" fill="var(--bg-card)" stroke="var(--accent-blue)" stroke-width="1.5" />
            <text class="svg-hover-badge-text" x="74" y="17" text-anchor="middle" fill="var(--ink-primary)" font-size="12" font-weight="700">(0, 0)</text>
          </g>
        </g>
      `
      : '';

    return `
      <div class="svg-plot-container">
        <svg class="svg-plot ${polyOrNull ? 'svg-plot-interactive' : ''}" viewBox="0 0 ${width} ${height}" role="img" aria-label="${ariaLabel}" ${polyDataAttrs}>
          ${gridLines}
          ${axesHTML}
          ${tickLabels}
          ${curveHTML}
          ${pointsHTML}
          ${interactiveOverlayHTML}
        </svg>
        ${legendHTML}
      </div>
    `;
  }

  // ===========================================================================
  // 5. Construcción de los pasos pedagógicos con el lenguaje del profesor
  // ===========================================================================
  function generateAllStepsData(workNodes, allPlaneNodes = null, evalPoint = null) {
    const planeNodes = allPlaneNodes || workNodes;
    const totalNodes = workNodes.length; // n + 1 nodos con los que trabajamos
    const totalPlane = planeNodes.length;
    const n = totalNodes - 1;            // grado máximo n
    const steps = [];

    // PASO 1: Los nodos en el plano
    const showExtraPlaneCol = totalPlane > totalNodes;
    let workCounter = 0;

    const rowsHTML = planeNodes.map((pt) => {
      const isWork = pt.active !== false;
      const idxLabel = isWork ? `i = ${workCounter}` : 'En el plano';
      const xSub = isWork ? `x<sub>${workCounter}</sub> = ${renderFracHTML(pt.x)}` : `x = ${renderFracHTML(pt.x)}`;
      const ySub = isWork ? `y<sub>${workCounter}</sub> = ${renderFracHTML(pt.y)}` : `y = ${renderFracHTML(pt.y)}`;
      if (isWork) workCounter++;
      return `
        <tr${!isWork ? ' style="opacity: 0.65;"' : ''}>
          <td class="node-index-cell">${idxLabel}</td>
          <td><span class="math-inline">${xSub}</span></td>
          <td><span class="math-inline">${ySub}</span></td>
          <td><span class="math-inline">(${pt.x.toPlainString()}, ${pt.y.toPlainString()})</span></td>
          ${showExtraPlaneCol ? `<td>${isWork ? '<strong>Se trabaja</strong>' : 'Solo en el plano'}</td>` : ''}
        </tr>
      `;
    }).join('');

    const svgNodesOnly = buildCartesianSVG(
      planeNodes,
      null,
      'Plano cartesiano con los nodos de interpolación'
    );

    const introExplanation = showExtraPlaneCol
      ? `En nuestro plano ubicamos <strong>${totalPlane} nodos</strong> y elegimos trabajar con <strong><span class="math-inline">n + 1 = ${totalNodes}</span> nodos</strong> (por lo tanto, <span class="math-inline">n = ${n}</span>). Así que buscamos un polinomio <span class="math-inline">P(x)</span> de <strong>grado <span class="math-inline">n = ${n}</span></strong> que pase por esos ${totalNodes} nodos de trabajo:`
      : `Tenemos <strong><span class="math-inline">n + 1 = ${totalNodes}</span> nodos</strong> (es decir, <span class="math-inline">n = ${n}</span>). Por definición, buscamos un polinomio <span class="math-inline">P(x)</span> de <strong>grado <span class="math-inline">n = ${n}</span></strong> que pase exactamente por todos estos puntos:`;

    steps.push({
      title: 'Los nodos en el plano',
      html: `
        <p class="step-explanation">
          ${introExplanation}
        </p>
        <div class="table-wrapper">
          <table class="nodes-summary-table" aria-label="Tabla de nodos">
            <thead>
              <tr>
                <th scope="col">Índice</th>
                <th scope="col">Abscisa <span class="math-inline">x<sub>i</sub></span></th>
                <th scope="col">Ordenada <span class="math-inline">y<sub>i</sub></span></th>
                <th scope="col">Punto <span class="math-inline">(x, y)</span></th>
                ${showExtraPlaneCol ? '<th scope="col">Estado</th>' : ''}
              </tr>
            </thead>
            <tbody>${rowsHTML}</tbody>
          </table>
        </div>
        ${svgNodesOnly}
      `
    });

    // PASO 2: Planteamos P(x)
    const sumTermsSymbolic = workNodes.map((_, i) => `L<sub>${i}</sub>(x)Y<sub>${i}</sub>`).join(' + ');

    const generalFormulasHTML = workNodes.map((_, i) => {
      const numFactors = [];
      const denFactors = [];
      for (let j = 0; j < totalNodes; j++) {
        if (j === i) continue;
        numFactors.push(`(x ${MINUS} x<sub>${j}</sub>)`);
        denFactors.push(`(x<sub>${i}</sub> ${MINUS} x<sub>${j}</sub>)`);
      }
      return `
        <div class="math-block">
          L<sub>${i}</sub>(x) =
          <span class="frac">
            <span class="frac-num">${numFactors.join('')}</span>
            <span class="frac-bar">/</span>
            <span class="frac-den">${denFactors.join('')}</span>
          </span>
        </div>
      `;
    }).join('');

    steps.push({
      title: 'Planteamos P(x)',
      html: `
        <p class="step-explanation">
          Planteamos nuestro polinomio <span class="math-inline">P(x)</span> sumando cada polinomio base <span class="math-inline">L<sub>i</sub>(x)</span> multiplicado por el valor <span class="math-inline">Y<sub>i</sub></span> de su nodo:
        </p>
        <div class="math-block">
          P(x) = ${sumTermsSymbolic}
        </div>
        <p class="step-explanation">
          Para armar cada fracción <span class="math-inline">L<sub>i</sub>(x)</span> el truco es muy sencillo: <strong>arriba</strong> ponemos <span class="math-inline">x</span> restando a todos los demás nodos, excepto al <span class="math-inline">x<sub>i</sub></span> que estamos calculando; y <strong>abajo</strong> escribimos exactamente lo mismo, solo que en vez de poner <span class="math-inline">x</span> al inicio, ponemos ese <span class="math-inline">x<sub>i</sub></span>:
        </p>
        ${generalFormulasHTML}
      `
    });

    // PASO 3 (uno por cada L_i(x))
    const basisPolys = [];

    for (let i = 0; i < totalNodes; i++) {
      const xi = workNodes[i].x;
      const otherNodes = [];
      const otherIndices = [];
      for (let j = 0; j < totalNodes; j++) {
        if (j !== i) {
          otherNodes.push(workNodes[j].x);
          otherIndices.push(j);
        }
      }

      const numFactorsHTML = otherNodes
        .map(xj => renderNumeratorFactorHTML(xj, otherNodes.length))
        .join('');

      const numFactorsColoredHTML = otherNodes
        .map(xj => renderNumeratorFactorHTML(xj, otherNodes.length, true))
        .join('');

      const denSubFactorsHTML = otherNodes.map(xj => {
        const xiStr = `<span class="coeff-highlight" title="Nodo propio x${i}">${renderFracHTML(xi, { parensIfNeg: true })}</span>`;
        const xjStr = `<span class="sub-x-highlight" title="Otro nodo xⱼ">${renderFracHTML(xj, { parensIfNeg: true })}</span>`;
        return `(${xiStr} ${MINUS} ${xjStr})`;
      }).join('');

      const denDiffs = otherNodes.map(xj => xi.sub(xj));
      let D = new Frac(1n);
      for (const diff of denDiffs) {
        D = D.mul(diff);
      }

      const denDiffsHTML = denDiffs
        .map(d => `(${renderFracHTML(d)})`)
        .join('');

      const denStepsChain = otherNodes.length > 1
        ? `D = ${denSubFactorsHTML} = ${denDiffsHTML} = <span class="coeff-highlight">${renderFracHTML(D)}</span>`
        : `D = ${denSubFactorsHTML} = <span class="coeff-highlight">${renderFracHTML(D)}</span>`;

      let monicPoly = [new Frac(1n)];
      for (const xj of otherNodes) {
        monicPoly = polyMul(monicPoly, [xj.neg(), new Frac(1n)]);
      }

      const invD = new Frac(1n).div(D);
      const LiPoly = polyScale(monicPoly, invD);
      basisPolys.push(LiPoly);

      const isDOne = D.isOne();
      const outerFracHTML = isDOne ? '' : `(${renderFracHTML(invD)})`;
      const outerFracColoredHTML = isDOne ? '' : `<span class="coeff-highlight" title="1/D">(${renderFracHTML(invD)})</span>`;

      const chainStages = [];
      const rawChainStages = [];
      function pushStage(strRaw, strStyled) {
        if (rawChainStages.length === 0 || rawChainStages[rawChainStages.length - 1] !== strRaw) {
          rawChainStages.push(strRaw);
          chainStages.push(`<span class="math-eq-stage">${strStyled || strRaw}</span>`);
        }
      }

      if (otherNodes.length === 1 && isDOne) {
        pushStage(renderPolyHTML(LiPoly));
      } else {
        pushStage(`${outerFracHTML}${numFactorsHTML}`, `${outerFracColoredHTML}${numFactorsColoredHTML}`);
      }

      if (otherNodes.length >= 2) {
        const unsimplifiedFirstTwo = renderUnsimplifiedTwoFactorsHTML(otherNodes[0], otherNodes[1]);
        const remainingFactorsHTML = otherNodes
          .slice(2)
          .map(xj => renderNumeratorFactorHTML(xj, otherNodes.length))
          .join('');
        if (isDOne && remainingFactorsHTML === '') {
          pushStage(`${unsimplifiedFirstTwo}`);
        } else {
          pushStage(`${outerFracHTML}(${unsimplifiedFirstTwo})${remainingFactorsHTML}`);
        }

        const simplifiedMonicHTML = renderPolyHTML(monicPoly);
        if (isDOne) {
          pushStage(`${simplifiedMonicHTML}`);
        } else {
          pushStage(`${outerFracHTML}(${simplifiedMonicHTML})`);
        }
      }

      if (!isDOne) {
        pushStage(renderPolyHTML(LiPoly));
      }

      const fullChainHTML = `<span class="math-eq-stage">L<sub>${i}</sub>(x)</span> = ` + chainStages.join(' = ');

      // Explicación (?) para a) Sustitución en L_i(x)
      const whySubA = renderWhyBoxHTML(
        `¿Por qué quedó así la sustitución en L${i}(x)?`,
        `
          <div>Como estamos calculando <strong><span class="math-inline">L<sub>${i}</sub>(x)</span></strong>, el nodo propio es <span class="math-inline">x<sub>${i}</sub> = ${renderFracHTML(xi)}</span> y usamos todos los demás nodos (<span class="math-inline">${otherIndices.map(j => `x<sub>${j}</sub> = ${renderFracHTML(workNodes[j].x)}`).join(', ')}</span>):</div>
          <ul class="why-sublist">
            <li><strong>Arriba (numerador):</strong> escribimos <span class="math-inline">(x ${MINUS} x<sub>j</sub>)</span> para cada nodo <span class="math-inline">j ≠ ${i}</span>. Si <span class="math-inline">x<sub>j</sub></span> es negativo, menos por menos da más y queda <span class="math-inline">(x + |x<sub>j</sub>|)</span>.</li>
            <li><strong>Abajo (denominador):</strong> copiamos exactamente la misma estructura de arriba, pero reemplazando la letra <span class="math-inline">x</span> por el valor fijo <span class="math-inline">x<sub>${i}</sub> = ${renderFracHTML(xi)}</span>.</li>
          </ul>
        `
      );

      // Explicación (?) para b) Denominador D
      const denEachBullets = otherNodes.map((xj, idx) => {
        const jIdx = otherIndices[idx];
        const diff = denDiffs[idx];
        const signNote = xj.isNegative()
          ? ` (como restamos un negativo, menos por menos da más: <span class="math-inline">${renderFracHTML(xi)} + ${renderFracHTML(xj.abs())}</span>)`
          : '';
        return `<li>Restamos <span class="math-inline">x<sub>${i}</sub> ${MINUS} x<sub>${jIdx}</sub> = ${renderFracHTML(xi, { parensIfNeg: true })} ${MINUS} ${renderFracHTML(xj, { parensIfNeg: true })}${signNote} = <strong>${renderFracHTML(diff)}</strong></span>.</li>`;
      }).join('');

      const whySubB = renderWhyBoxHTML(
        '¿Cómo se restó y multiplicó el denominador D?',
        `
          <div><strong>1. Resolvemos cada paréntesis del denominador por separado:</strong></div>
          <ul class="why-sublist">${denEachBullets}</ul>
          <div><strong>2. Multiplicamos esos resultados entre sí:</strong> <span class="math-inline">${denDiffsHTML} = <strong>${renderFracHTML(D)}</strong></span>.</div>
          <div><strong>3. ¿Por qué sale <span class="math-inline">${renderFracHTML(invD)}</span> afuera?:</strong> dividir un polinomio entre <span class="math-inline">D = ${renderFracHTML(D)}</span> es exactamente lo mismo que multiplicar por la fracción <span class="math-inline">1/D = <strong>${renderFracHTML(invD)}</strong></span>.</div>
        `
      );

      // Explicación (?) para c) Multiplicación del numerador y distribución de 1/D
      const whySubC = renderWhyBoxHTML(
        '¿Cómo se multiplicaron los paréntesis y la fracción paso a paso?',
        buildFactorMultiplicationBreakdownHTML(otherNodes, otherIndices, invD, '1/D', false)
      );

      steps.push({
        title: `Calculamos L<sub>${i}</sub>(x)`,
        html: `
          <ul class="substep-list">
            <li class="substep-item">
              <span class="substep-label">a) Reemplazamos los valores de <span class="math-inline">x</span> en la fracción (en <span class="coeff-highlight" style="font-size:0.85em;">rojo</span> el nodo propio <span class="math-inline">x<sub>${i}</sub></span> y en <span class="sub-x-highlight" style="font-size:0.85em;">verde</span> los demás nodos):</span>
              <div class="math-block">
                L<sub>${i}</sub>(x) =
                <span class="frac">
                  <span class="frac-num">${numFactorsColoredHTML}</span>
                  <span class="frac-bar">/</span>
                  <span class="frac-den">${denSubFactorsHTML}</span>
                </span>
              </div>
              ${whySubA}
            </li>
            <li class="substep-item">
              <span class="substep-label">b) Resolvemos primero el denominador numéricamente (<span class="math-inline">D</span>):</span>
              <div class="math-block">
                ${denStepsChain}
              </div>
              ${whySubB}
            </li>
            <li class="substep-item">
              <span class="substep-label">c) ${isDOne ? 'Como <span class="math-inline">D = 1</span>, resolvemos el numerador en una sola cadena de igualdades:' : 'Dejamos la fracción <span class="math-inline">1/D</span> afuera y desarrollamos el numerador en una sola cadena de igualdades:'}</span>
              <div class="math-block">
                ${fullChainHTML}
              </div>
              ${whySubC}
            </li>
          </ul>
        `
      });
    }

    // PASO 4: Multiplicamos cada L por su y
    const scaledPolys = workNodes.map((pt, i) => polyScale(basisPolys[i], pt.y));

    const pCombinedWithY = workNodes.map((pt, i) => {
      const yHTML = `<span class="coeff-highlight" title="Coordenada y${i} reemplazada">${renderFracHTML(pt.y, { parensIfNeg: true })}</span>`;
      const lHTML = `(${renderPolyHTML(basisPolys[i])})`;
      return `${yHTML}${lHTML}`;
    }).join(' + ');

    const whyStep4Sub = renderWhyBoxHTML(
      '¿Por qué sustituimos así cada Yᵢ · Lᵢ(x)?',
      `
        <div>En la fórmula <span class="math-inline">P(x) = L<sub>0</sub>(x)Y<sub>0</sub> + L<sub>1</sub>(x)Y<sub>1</sub> + …</span> reemplazamos cada <span class="math-inline">Y<sub>i</sub></span> por la coordenada <span class="math-inline">y</span> de su nodo y cada <span class="math-inline">L<sub>i</sub>(x)</span> por el polinomio que acabamos de hallar:</div>
        <ul class="why-sublist">
          ${workNodes.map((pt, i) => `<li>Para el nodo <span class="math-inline">i = ${i}</span>: <span class="math-inline">y<sub>${i}</sub> = <strong>${renderFracHTML(pt.y)}</strong></span> multiplica a <span class="math-inline">L<sub>${i}</sub>(x) = <strong>(${renderPolyHTML(basisPolys[i])})</strong></span>${pt.y.isNegative() ? ' (se encierra entre paréntesis por ser negativo)' : ''}.</li>`).join('')}
        </ul>
      `
    );

    const eachDistributedHTML = workNodes.map((pt, i) => {
      const yHTML = renderFracHTML(pt.y, { parensIfNeg: true });
      const yBadge = `<span class="coeff-highlight" title="Coordenada y${i} reemplazada">${yHTML}</span>`;
      const lHTML = `(${renderPolyHTML(basisPolys[i])})`;
      const resHTML = renderPolyHTML(scaledPolys[i]);

      const termProdBullets = [];
      for (let d = basisPolys[i].length - 1; d >= 0; d--) {
        const c = basisPolys[i][d];
        if (c.isZero()) continue;
        const prod = c.mul(pt.y);
        termProdBullets.push(`<li>Multiplicamos <span class="math-inline">${yHTML} · (${renderTermWithSignHTML(c, d, true)}) = <strong>${renderTermWithSignHTML(prod, d, true)}</strong></span></li>`);
      }

      const whyDistI = renderWhyBoxHTML(
        `¿Cómo se multiplicó Y<sub>${i}</sub> = ${pt.y.toPlainString()} por cada término de L<sub>${i}</sub>(x)?`,
        `
          <div>Aplicamos la propiedad distributiva multiplicando <span class="math-inline">y<sub>${i}</sub> = ${renderFracHTML(pt.y)}</span> por cada término dentro del paréntesis <span class="math-inline">(${renderPolyHTML(basisPolys[i])})</span>:</div>
          <ul class="why-sublist">${termProdBullets.join('')}</ul>
          <div>Al unir los resultados obtenemos <span class="math-inline"><strong>${resHTML}</strong></span>.</div>
        `
      );

      return `
        <div class="math-block">
          <span class="math-eq-stage">Y<sub>${i}</sub> · L<sub>${i}</sub>(x)</span> = <span class="math-eq-stage">${yBadge}${lHTML}</span> = <span class="math-eq-stage">${resHTML}</span>
        </div>
        ${whyDistI}
      `;
    }).join('');

    steps.push({
      title: 'Multiplicamos cada L por su y',
      html: `
        <p class="step-explanation">
          Ahora que ya tenemos cada <span class="math-inline">L<sub>i</sub>(x)</span> simplificado, lo multiplicamos por la coordenada <span class="math-inline">y<sub>i</sub></span> de su nodo:
        </p>
        <div class="math-block">
          P(x) = ${pCombinedWithY}
        </div>
        ${whyStep4Sub}
        <p class="step-explanation">
          Distribuimos cada <span class="math-inline">Y<sub>i</sub></span> término a término:
        </p>
        ${eachDistributedHTML}
      `
    });

    // PASO 5: Sumamos
    let finalPoly = [new Frac(0n)];
    for (const sp of scaledPolys) {
      finalPoly = polyAdd(finalPoly, sp);
    }

    const sumOfPolysHTML = scaledPolys
      .map(sp => `(${renderPolyHTML(sp)})`)
      .join(' + ');

    const groupedByDegreeLines = [];
    const groupedInlineParts = [];
    const lagrangeSourceLabels = workNodes.map((_, i) => `Y<sub>${i}</sub> · L<sub>${i}</sub>(x)`);

    for (let deg = n; deg >= 0; deg--) {
      const coeffsAtDeg = scaledPolys.map(sp => (deg < sp.length ? sp[deg] : new Frac(0n)));
      const nonZeroCoeffs = coeffsAtDeg.filter(c => !c.isZero());

      const sumExpr = nonZeroCoeffs.length > 0
        ? nonZeroCoeffs.map((c, idx) => renderTermWithSignHTML(c, 0, idx === 0)).join('')
        : '0';
      const needsParens = deg > 0 && (nonZeroCoeffs.length > 1 || (nonZeroCoeffs.length === 1 && nonZeroCoeffs[0].isNegative()));
      const wrappedSumExpr = needsParens ? `(${sumExpr})` : sumExpr;

      const totalAtDeg = deg < finalPoly.length ? finalPoly[deg] : new Frac(0n);
      const varLabel = deg === 0 ? '' : (deg === 1 ? 'x' : `x<sup>${deg}</sup>`);
      const degTitle = deg === 0
        ? 'Término independiente'
        : (deg === 1 ? 'Términos con x' : `Términos con x<sup>${deg}</sup>`);

      const whyDeg = renderWhyBoxHTML(
        `¿De dónde salen estos números y cómo se suman? (${degTitle})`,
        buildDegreeGroupingBreakdownHTML(deg, scaledPolys, lagrangeSourceLabels, totalAtDeg)
      );

      groupedByDegreeLines.push(`
        <div class="math-block">
          <span style="font-style: normal; font-weight: 600; color: var(--ink-secondary);">${degTitle}:</span>
          <span class="math-eq-stage">${wrappedSumExpr}${varLabel}</span> = <span class="math-eq-stage">${renderTermWithSignHTML(totalAtDeg, deg, true)}</span>
        </div>
        ${whyDeg}
      `);

      if (nonZeroCoeffs.length > 0) {
        groupedInlineParts.push(`(${sumExpr})${varLabel}`);
      }
    }

    const finalPolyHTML = renderPolyHTML(finalPoly);

    steps.push({
      title: 'Sumamos',
      html: `
        <p class="step-explanation">
          Sumamos todos los resultados anteriores y agrupamos términos semejantes (las <span class="math-inline">x</span> del mismo grado con las del mismo grado):
        </p>
        <div class="math-block">
          P(x) = ${sumOfPolysHTML}
        </div>
        ${groupedInlineParts.length > 0 ? `<div class="math-block">P(x) = ${groupedInlineParts.join(' + ')}</div>` : ''}
        ${groupedByDegreeLines.join('')}
        <p class="step-explanation">
          ¡Listo! Este es nuestro <strong>polinomio de Lagrange final</strong>:
        </p>
        <div class="math-block math-final-highlight" id="final-polynomial-result">
          P(x) = ${finalPolyHTML}
        </div>
      `
    });

    // PASO 6: Verificamos con la gráfica
    const verificationItemsHTML = workNodes.map((pt, i) => {
      const xVal = pt.x;
      const evalSubHTML = renderPolyEvaluationSubstitutionHTML(finalPoly, xVal);
      const evaluatedY = polyEval(finalPoly, xVal);
      const whyEval = renderWhyBoxHTML(
        `¿Cómo se calculó P(${xVal.toPlainString()}) paso a paso?`,
        buildPolyEvaluationBreakdownHTML(finalPoly, xVal)
      );

      return `
        <li class="verification-item" style="flex-direction: column; align-items: stretch;">
          <div style="display: flex; align-items: center; justify-content: space-between; gap: 0.75rem; flex-wrap: wrap;">
            <span class="math-inline">
              <strong>Nodo ${i}</strong> (${pt.x.toPlainString()}, ${pt.y.toPlainString()}):
              &nbsp; P(${renderFracHTML(xVal)}) = ${evalSubHTML} = <strong>${renderFracHTML(evaluatedY)}</strong> = y<sub>${i}</sub>
            </span>
            <span class="check-badge" aria-label="Verificado">✓ Cumple</span>
          </div>
          ${whyEval}
        </li>
      `;
    }).join('');

    const svgCurveAndNodes = buildCartesianSVG(
      planeNodes,
      finalPoly,
      'Gráfica del polinomio de Lagrange pasando por los nodos',
      evalPoint
    );

    steps.push({
      title: 'Verificamos con la gráfica',
      html: `
        <p class="step-explanation">
          Para estar seguros de que no nos equivocamos en las cuentas, comprobamos que <span class="math-inline">P(x<sub>i</sub>) = y<sub>i</sub></span> en cada nodo con el que trabajamos:
        </p>
        <ul class="verification-list">
          ${verificationItemsHTML}
        </ul>
        <p class="step-explanation">
          Y al graficar la curva de <span class="math-inline">P(x)</span> en el plano cartesiano, vemos cómo pasa exactamente por los <span class="math-inline">${totalNodes}</span> nodos:
        </p>
        ${svgCurveAndNodes}
      `
    });

    return steps;
  }

  // ===========================================================================
  // 6. Método de Diferencias Divididas (Tabla Vertical dibujada paso a paso)
  // ===========================================================================
  function computeDividedDifferencesTable(workNodes) {
    const N = workNodes.length;
    const ddTable = [];

    // Orden 0: columna y_i
    ddTable[0] = workNodes.map((pt, i) => ({
      order: 0,
      i,
      val: pt.y
    }));

    // Órdenes 1 hasta N - 1
    for (let order = 1; order < N; order++) {
      ddTable[order] = [];
      for (let i = 0; i < N - order; i++) {
        const bottomVal = ddTable[order - 1][i + 1].val;
        const topVal = ddTable[order - 1][i].val;
        const numDiff = bottomVal.sub(topVal);

        const idxBottom = i + order;
        const idxTop = i;
        const xBottom = workNodes[idxBottom].x;
        const xTop = workNodes[idxTop].x;
        const denDiff = xBottom.sub(xTop);

        const val = numDiff.div(denDiff);
        ddTable[order].push({
          order,
          i,
          val,
          bottomVal,
          topVal,
          numDiff,
          idxBottom,
          idxTop,
          xBottom,
          xTop,
          denDiff
        });
      }
    }

    return ddTable;
  }

  function computeDividedDifferencesPolynomial(workNodes) {
    const N = workNodes.length;
    const ddTable = computeDividedDifferencesTable(workNodes);
    const coeffs = [];
    let finalPoly = [new Frac(0n)];

    for (let k = 0; k < N; k++) {
      const ak = ddTable[k][0].val;
      coeffs.push(ak);
      let monicPoly = [new Frac(1n)];
      for (let j = 0; j < k; j++) {
        monicPoly = polyMul(monicPoly, [workNodes[j].x.neg(), new Frac(1n)]);
      }
      const termPoly = polyScale(monicPoly, ak);
      finalPoly = polyAdd(finalPoly, termPoly);
    }

    return { ddTable, coeffs, finalPoly };
  }

  // Genera el diagrama SVG en verde de la Tabla Vertical paso a paso (con líneas de fracción reales)
  function buildDividedDiffVisualSVG(workNodes, ddTable, maxVisibleOrder, highlightOrder = null, highlightRowIdx = null, highlightTopDiagonal = false) {
    const N = workNodes.length;
    const colWidth = 172;
    const xColStart = 92;
    const yColStart = 188;
    const totalCols = maxVisibleOrder + 1;
    const width = Math.max(640, yColStart + totalCols * colWidth + 36);
    const rowSpacing = 86;
    const topPad = 68;
    const height = topPad + (N - 1) * rowSpacing + 72;

    function getRowY(logicalRow) {
      return topPad + logicalRow * rowSpacing;
    }

    function getColX(order) {
      return yColStart + order * colWidth;
    }

    const defs = `
      <defs>
        <marker id="arrow-green" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="6" markerHeight="6" orient="auto-start-reverse">
          <path d="M 0 1 L 10 5 L 0 9 z" fill="var(--check-green)" />
        </marker>
        <marker id="arrow-muted" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="5" markerHeight="5" orient="auto-start-reverse">
          <path d="M 0 1 L 10 5 L 0 9 z" fill="var(--ink-muted)" />
        </marker>
      </defs>
    `;

    // Encabezados de la tabla vertical
    let headers = `
      <line x1="16" y1="38" x2="${width - 16}" y2="38" stroke="var(--border-strong)" stroke-width="1.5" />
      <text x="34" y="26" text-anchor="middle" fill="var(--ink-secondary)" font-size="13" font-weight="700">i</text>
      <text x="${xColStart}" y="26" text-anchor="middle" fill="var(--ink-secondary)" font-size="14" font-weight="700" font-style="italic">xᵢ</text>
      <text x="${getColX(0)}" y="26" text-anchor="middle" fill="var(--ink-secondary)" font-size="14" font-weight="700" font-style="italic">yᵢ</text>
    `;

    for (let ord = 1; ord <= maxVisibleOrder; ord++) {
      const cx = getColX(ord);
      const isHi = highlightOrder === ord;
      const fillCol = isHi ? 'var(--check-green)' : 'var(--ink-secondary)';
      headers += `
        <text x="${cx}" y="26" text-anchor="middle" fill="${fillCol}" font-size="13" font-weight="700">
          ${ord}.ª Dif. Dividida
        </text>
      `;
    }

    // Líneas verticales separadoras
    const gridLines = `
      <line x1="56" y1="10" x2="56" y2="${height - 16}" stroke="var(--border-light)" stroke-width="1" stroke-dasharray="3 3" />
      <line x1="132" y1="10" x2="132" y2="${height - 16}" stroke="var(--border-strong)" stroke-width="1.5" />
    `;

    // Llave en verde en la columna x_i para el par de x que se está restando en el paso actual
    let xBrackets = '';
    let activeCell = null;
    if (highlightOrder && highlightOrder <= maxVisibleOrder && highlightRowIdx !== null) {
      activeCell = ddTable[highlightOrder][highlightRowIdx];
      if (activeCell) {
        const yTopX = getRowY(activeCell.idxTop);
        const yBotX = getRowY(activeCell.idxBottom);
        const bx = xColStart - 26;
        xBrackets += `
          <path d="M ${xColStart - 15} ${yTopX} L ${bx} ${yTopX} L ${bx} ${yBotX} L ${xColStart - 15} ${yBotX}"
                fill="none" stroke="var(--check-green)" stroke-width="2.2" stroke-dasharray="3 2" />
          <circle cx="${xColStart}" cy="${yTopX}" r="16" fill="var(--check-green-bg)" stroke="var(--check-green)" stroke-width="1.8" />
          <circle cx="${xColStart}" cy="${yBotX}" r="16" fill="var(--check-green-bg)" stroke="var(--check-green)" stroke-width="1.8" />
        `;
      }
    }

    // Filas base: i y x_i
    let baseNodesSVG = '';
    for (let i = 0; i < N; i++) {
      const cy = getRowY(i);
      baseNodesSVG += `
        <text x="34" y="${cy + 5}" text-anchor="middle" fill="var(--ink-muted)" font-size="13">i=${i}</text>
        <g class="dd-svg-x-node" data-dd-x="${i}">
          <circle cx="${xColStart}" cy="${cy}" r="16" fill="var(--check-green-bg)" stroke="var(--check-green)" stroke-width="2" opacity="0" />
          <text x="${xColStart}" y="${cy + 5}" text-anchor="middle" fill="var(--ink-primary)" font-size="15" font-weight="700">${workNodes[i].x.toPlainString()}</text>
        </g>
      `;
    }

    // Líneas conectoras entre los números que se restan y flechita hacia la siguiente resta
    let connectorsSVG = '';
    for (let ord = 1; ord <= maxVisibleOrder; ord++) {
      const prevX = getColX(ord - 1);
      const currX = getColX(ord);
      const maxRowForOrd = (highlightOrder === ord && highlightRowIdx !== null)
        ? highlightRowIdx
        : (N - ord - 1);

      for (let i = 0; i <= maxRowForOrd; i++) {
        const yTop = getRowY(i + (ord - 1) * 0.5);
        const yBottom = getRowY(i + 1 + (ord - 1) * 0.5);
        const yMid = getRowY(i + ord * 0.5);

        const startX = prevX + (ord - 1 === 0 ? 32 : 58);
        const endX = currX - 60;
        const isCurrentActivePair = (highlightOrder === ord && highlightRowIdx === i);

        if (isCurrentActivePair) {
          connectorsSVG += `
            <path d="M ${startX} ${yTop + 4} L ${endX} ${yMid - 8}"
                  fill="none" stroke="var(--check-green)" stroke-width="2.2" marker-end="url(#arrow-green)" />
            <path d="M ${startX} ${yBottom - 4} L ${endX} ${yMid + 8}"
                  fill="none" stroke="var(--check-green)" stroke-width="2.2" marker-end="url(#arrow-green)" />
          `;
          // Si no es la primera resta de la columna, dibujar una flechita desde la resta anterior hacia esta
          if (i > 0) {
            const prevCellY = getRowY((i - 1) + ord * 0.5);
            connectorsSVG += `
              <path d="M ${currX} ${prevCellY + 29} L ${currX} ${yMid - 30}"
                    fill="none" stroke="var(--check-green)" stroke-width="2.2" stroke-dasharray="4 2" marker-end="url(#arrow-green)" />
            `;
          }
        } else {
          connectorsSVG += `
            <path d="M ${startX} ${yTop + 3} L ${endX} ${yMid - 6}"
                  fill="none" stroke="var(--ink-muted)" stroke-width="1.2" stroke-dasharray="3 2" opacity="0.55" marker-end="url(#arrow-muted)" />
            <path d="M ${startX} ${yBottom - 3} L ${endX} ${yMid + 6}"
                  fill="none" stroke="var(--ink-muted)" stroke-width="1.2" stroke-dasharray="3 2" opacity="0.55" marker-end="url(#arrow-muted)" />
          `;
        }
      }
    }

    // Celdas de valores (con línea horizontal de fracción real dentro del SVG)
    let cellsSVG = '';
    for (let ord = 0; ord <= maxVisibleOrder; ord++) {
      const cx = getColX(ord);
      const maxRowForOrd = (highlightOrder === ord && highlightRowIdx !== null)
        ? highlightRowIdx
        : (N - ord - 1);

      for (let i = 0; i <= maxRowForOrd; i++) {
        const cell = ddTable[ord][i];
        const cy = getRowY(i + ord * 0.5);
        const isTopDiag = highlightTopDiagonal && i === 0;
        const isCurrentActiveCell = (highlightOrder === ord && highlightRowIdx === i);
        const isSourcePairForActive = (
          highlightOrder &&
          ord === highlightOrder - 1 &&
          highlightRowIdx !== null &&
          (i === highlightRowIdx || i === highlightRowIdx + 1)
        );

        let boxFill = 'var(--bg-card)';
        let boxStroke = 'var(--border-strong)';
        let strokeW = '1.4';

        if (isTopDiag) {
          boxFill = 'var(--accent-red-bg)';
          boxStroke = 'var(--accent-red)';
          strokeW = '2.4';
        } else if (isCurrentActiveCell || isSourcePairForActive) {
          boxFill = 'var(--check-green-bg)';
          boxStroke = 'var(--check-green)';
          strokeW = '2.2';
        }

        let cellInnerSVG = '';
        if (ord >= 1) {
          const bStr = cell.bottomVal.isNegative() ? `(${cell.bottomVal.toPlainString()})` : cell.bottomVal.toPlainString();
          const tStr = cell.topVal.isNegative() ? `(${cell.topVal.toPlainString()})` : cell.topVal.toPlainString();
          const xbStr = cell.xBottom.isNegative() ? `(${cell.xBottom.toPlainString()})` : cell.xBottom.toPlainString();
          const xtStr = cell.xTop.isNegative() ? `(${cell.xTop.toPlainString()})` : cell.xTop.toPlainString();

          const numText = `${bStr} ${MINUS} ${tStr}`;
          const denText = `${xbStr} ${MINUS} ${xtStr}`;
          const valText = `= ${cell.val.toPlainString()}`;

          // Caja con fracción vertical real (numerador arriba, línea horizontal, denominador abajo) y resultado al lado
          const fracCenterX = cx - 20;
          const resX = cx + 22;
          cellInnerSVG += `
            <rect x="${cx - 58}" y="${cy - 26}" width="116" height="52" rx="7" fill="${boxFill}" stroke="${boxStroke}" stroke-width="${strokeW}" />
            <text x="${fracCenterX}" y="${cy - 7}" text-anchor="middle" fill="var(--ink-primary)" font-size="11" font-weight="600">${numText}</text>
            <line x1="${fracCenterX - 32}" y1="${cy}" x2="${fracCenterX + 32}" y2="${cy}" stroke="var(--ink-primary)" stroke-width="1.5" />
            <text x="${fracCenterX}" y="${cy + 14}" text-anchor="middle" fill="var(--ink-primary)" font-size="11" font-weight="600">${denText}</text>
            <text x="${resX}" y="${cy + 5}" text-anchor="start" fill="var(--ink-primary)" font-size="13.5" font-weight="700">${valText}</text>
          `;
        } else {
          cellInnerSVG += `
            <rect x="${cx - 32}" y="${cy - 17}" width="64" height="34" rx="7" fill="${boxFill}" stroke="${boxStroke}" stroke-width="${strokeW}" />
            <text x="${cx}" y="${cy + 5}" text-anchor="middle" fill="var(--ink-primary)" font-size="15" font-weight="700">${cell.val.toPlainString()}</text>
          `;
        }

        if (isTopDiag) {
          cellInnerSVG += `
            <rect x="${cx - 18}" y="${cy - 41}" width="36" height="17" rx="4" fill="var(--accent-red)" />
            <text x="${cx}" y="${cy - 29}" text-anchor="middle" fill="#ffffff" font-size="11" font-weight="700">a${ord}</text>
          `;
        }

        cellsSVG += `
          <g class="dd-svg-cell-group" data-dd-ord="${ord}" data-dd-row="${i}">
            ${cellInnerSVG}
          </g>
        `;
      }
    }

    return `
      <div class="dd-diagram-wrapper">
        <svg class="dd-svg-board" viewBox="0 0 ${width} ${height}" role="img" aria-label="Tabla vertical escalonada de diferencias divididas">
          ${defs}
          ${headers}
          ${gridLines}
          ${xBrackets}
          ${baseNodesSVG}
          ${connectorsSVG}
          ${cellsSVG}
        </svg>
        <div class="plot-legend">
          <span class="legend-item"><span class="dd-pill-x" style="font-size:0.75rem;">Resaltado en verde</span> Para armar cada fracción: arriba restas los dos números de la columna anterior (el de abajo menos el de arriba) y abajo restas sus dos valores de xᵢ (el de abajo menos el de arriba).</span>
        </div>
      </div>
    `;
  }

  // Genera además la Tabla Vertical HTML con fracciones verticales exactas (.frac con línea visible)
  function buildDividedDiffHTMLTable(workNodes, ddTable, maxVisibleOrder, highlightTopDiagonal = false) {
    const N = workNodes.length;
    let ths = `
      <th scope="col">Fila <span class="math-inline">i</span></th>
      <th scope="col"><span class="math-inline">x<sub>i</sub></span></th>
      <th scope="col"><span class="math-inline">y<sub>i</sub></span></th>
    `;
    for (let ord = 1; ord <= maxVisibleOrder; ord++) {
      ths += `<th scope="col">${ord}.ª Dif. Dividida</th>`;
    }

    let trs = '';
    for (let i = 0; i < N; i++) {
      let tds = `
        <td class="node-index-cell">i = ${i}</td>
        <td data-dd-x="${i}"><strong class="math-inline">x<sub>${i}</sub> = ${renderFracHTML(workNodes[i].x)}</strong></td>
      `;

      const isA0 = highlightTopDiagonal && i === 0;
      tds += `
        <td class="${isA0 ? 'dd-cell-top-coeff' : ''}" data-dd-ord="0" data-dd-row="${i}">
          ${isA0 ? '<div class="dd-coeff-tag">a<sub>0</sub></div><br>' : ''}
          <strong class="math-inline">${renderFracHTML(ddTable[0][i].val)}</strong>
        </td>
      `;

      for (let ord = 1; ord <= maxVisibleOrder; ord++) {
        if (i < N - ord) {
          const cell = ddTable[ord][i];
          const isAk = highlightTopDiagonal && i === 0;
          const bHTML = renderFracHTML(cell.bottomVal, { parensIfNeg: true });
          const tHTML = renderFracHTML(cell.topVal, { parensIfNeg: true });
          const xbHTML = renderFracHTML(cell.xBottom, { parensIfNeg: true });
          const xtHTML = renderFracHTML(cell.xTop, { parensIfNeg: true });

          tds += `
            <td class="${isAk ? 'dd-cell-top-coeff' : ''}" data-dd-ord="${ord}" data-dd-row="${i}" style="cursor: pointer;" title="Pasa el mouse para iluminar qué números se restaron">
              ${isAk ? `<div class="dd-coeff-tag">a<sub>${ord}</sub></div><br>` : ''}
              <span class="math-inline">
                <span class="frac">
                  <span class="frac-num">${bHTML} ${MINUS} ${tHTML}</span>
                  <span class="frac-bar">/</span>
                  <span class="frac-den">${xbHTML} ${MINUS} ${xtHTML}</span>
                </span>
                = <strong>${renderFracHTML(cell.val)}</strong>
              </span>
            </td>
          `;
        } else {
          tds += `<td style="background-color: var(--bg-subtle); color: var(--ink-muted);">—</td>`;
        }
      }

      trs += `<tr>${tds}</tr>`;
    }

    return `
      <div class="table-wrapper">
        <table class="dd-vertical-table" aria-label="Tabla vertical de diferencias divididas">
          <thead><tr>${ths}</tr></thead>
          <tbody>${trs}</tbody>
        </table>
      </div>
    `;
  }

  // Construye todos los pasos del Método de Diferencias Divididas (resta por resta con flechita)
  function generateDividedDiffStepsData(workNodes, allPlaneNodes = null, evalPoint = null) {
    const planeNodes = allPlaneNodes || workNodes;
    const N = workNodes.length; // n + 1 nodos con los que trabajamos
    const totalPlane = planeNodes.length;
    const showExtraPlaneCol = totalPlane > N;
    const n = N - 1;
    const ddTable = computeDividedDifferencesTable(workNodes);
    const steps = [];

    // -------------------------------------------------------------------------
    // PASO 1: Los nodos y la fórmula de Newton por Diferencias Divididas
    // -------------------------------------------------------------------------
    let workCounter = 0;
    const rowsHTML = planeNodes.map((pt) => {
      const isWork = pt.active !== false;
      const idxLabel = isWork ? `i = ${workCounter}` : 'En el plano';
      const xSub = isWork ? `x<sub>${workCounter}</sub> = ${renderFracHTML(pt.x)}` : `x = ${renderFracHTML(pt.x)}`;
      const ySub = isWork ? `y<sub>${workCounter}</sub> = ${renderFracHTML(pt.y)}` : `y = ${renderFracHTML(pt.y)}`;
      if (isWork) workCounter++;
      return `
        <tr${!isWork ? ' style="opacity: 0.65;"' : ''}>
          <td class="node-index-cell">${idxLabel}</td>
          <td><span class="math-inline">${xSub}</span></td>
          <td><span class="math-inline">${ySub}</span></td>
          <td><span class="math-inline">(${pt.x.toPlainString()}, ${pt.y.toPlainString()})</span></td>
          ${showExtraPlaneCol ? `<td>${isWork ? '<strong>Se trabaja</strong>' : 'Solo en el plano'}</td>` : ''}
        </tr>
      `;
    }).join('');

    const newtonFormulaSymbolic = workNodes.map((_, k) => {
      if (k === 0) return 'a<sub>0</sub>';
      const factors = [];
      for (let j = 0; j < k; j++) {
        factors.push(`(x ${MINUS} x<sub>${j}</sub>)`);
      }
      return `a<sub>${k}</sub>${factors.join('')}`;
    }).join(' + ');

    const introExplanation = showExtraPlaneCol
      ? `En nuestro plano ubicamos <strong>${totalPlane} nodos</strong> y elegimos trabajar con <strong><span class="math-inline">n + 1 = ${N}</span> nodos</strong> (así que buscamos un polinomio de grado <span class="math-inline">n = ${n}</span>). En el <strong>Método de Diferencias Divididas</strong> planteamos el polinomio de esta forma:`
      : `Tenemos <strong><span class="math-inline">n + 1 = ${N}</span> nodos</strong> (así que buscamos un polinomio de grado <span class="math-inline">n = ${n}</span>). En el <strong>Método de Diferencias Divididas</strong> planteamos el polinomio de esta forma:`;

    const svgInitialPlane = allPlaneNodes
      ? buildCartesianSVG(planeNodes, null, 'Plano cartesiano con los nodos de interpolación')
      : '';

    steps.push({
      title: 'Los nodos y la fórmula de Diferencias Divididas',
      html: `
        <p class="step-explanation">
          ${introExplanation}
        </p>
        <div class="math-block">
          P(x) = ${newtonFormulaSymbolic}
        </div>
        <p class="step-explanation">
          Para hallar los coeficientes <span class="math-inline">a<sub>0</sub>, a<sub>1</sub>, …, a<sub>${n}</sub></span>, construimos una <strong>tablita por columnas</strong> con nuestros ${N} nodos de trabajo:
        </p>
        <div class="table-wrapper">
          <table class="nodes-summary-table" aria-label="Tabla de nodos iniciales">
            <thead>
              <tr>
                <th scope="col">Índice</th>
                <th scope="col">Abscisa <span class="math-inline">x<sub>i</sub></span></th>
                <th scope="col">Ordenada <span class="math-inline">y<sub>i</sub></span></th>
                <th scope="col">Punto <span class="math-inline">(x, y)</span></th>
                ${showExtraPlaneCol ? '<th scope="col">Estado</th>' : ''}
              </tr>
            </thead>
            <tbody>${rowsHTML}</tbody>
          </table>
        </div>
        ${svgInitialPlane}
      `
    });

    // -------------------------------------------------------------------------
    // PASO 2: Armamos la Tabla Vertical inicial y la regla de cómo se resta
    // -------------------------------------------------------------------------
    steps.push({
      title: 'Armamos la Tabla Vertical y cómo se resta',
      html: `
        <p class="step-explanation">
          Anotamos en vertical la columna de las <span class="math-inline">x<sub>i</sub></span> y al lado la columna de las <span class="math-inline">y<sub>i</sub></span>. Para calcular cada columna nueva, recuerda siempre cómo se resta:
        </p>
        <div class="dd-sub-explain-card">
          <p style="margin: 0 0 0.45rem 0;">
            <strong>1. Arriba de la fracción (numerador):</strong> miramos la columna anterior y <strong>el número de abajo se resta con el de arriba</strong>.
          </p>
          <p style="margin: 0;">
            <strong>2. Abajo de la fracción (denominador):</strong> miramos la columna <span class="math-inline">x<sub>i</sub></span> y <strong>la x de abajo se resta con la x de arriba</strong>.
          </p>
        </div>
        ${buildDividedDiffVisualSVG(workNodes, ddTable, 0, null, null, false)}
      `
    });

    // -------------------------------------------------------------------------
    // PASOS DETALLADOS: Cada resta paso a paso dentro de cada Diferencia Dividida
    // -------------------------------------------------------------------------
    for (let ord = 1; ord <= n; ord++) {
      const colCells = ddTable[ord];
      const prevColName = ord === 1 ? 'la columna y<sub>i</sub>' : `la ${(ord - 1)}.ª Diferencia Dividida`;

      for (let idx = 0; idx < colCells.length; idx++) {
        const cell = colCells[idx];
        const bHTML = renderFracHTML(cell.bottomVal, { parensIfNeg: true });
        const tHTML = renderFracHTML(cell.topVal, { parensIfNeg: true });
        const xbHTML = renderFracHTML(cell.xBottom, { parensIfNeg: true });
        const xtHTML = renderFracHTML(cell.xTop, { parensIfNeg: true });

        // Cadena progresiva con flechitas (→) mostrando las restas halladas hasta este momento en esta columna
        const chainItemsHTML = [];
        for (let k = 0; k <= idx; k++) {
          const cItem = colCells[k];
          const kb = renderFracHTML(cItem.bottomVal, { parensIfNeg: true });
          const kt = renderFracHTML(cItem.topVal, { parensIfNeg: true });
          const kxb = renderFracHTML(cItem.xBottom, { parensIfNeg: true });
          const kxt = renderFracHTML(cItem.xTop, { parensIfNeg: true });
          const isCurrent = k === idx;

          chainItemsHTML.push(`
            <div class="dd-chain-step ${isCurrent ? 'active-step' : ''}">
              <span class="frac">
                <span class="frac-num">${kb} ${MINUS} ${kt}</span>
                <span class="frac-bar">/</span>
                <span class="frac-den">${kxb} ${MINUS} ${kxt}</span>
              </span>
              <span>=</span>
              <span class="frac">
                <span class="frac-num">${renderFracHTML(cItem.numDiff)}</span>
                <span class="frac-bar">/</span>
                <span class="frac-den">${renderFracHTML(cItem.denDiff)}</span>
              </span>
              <span>= <strong>${renderFracHTML(cItem.val)}</strong></span>
            </div>
          `);
        }

        const progressiveChainWithArrows = chainItemsHTML.join('<span class="dd-chain-arrow" aria-label="siguiente resta">→</span>');

        const stepSubtitle = colCells.length > 1
          ? `Calculamos la ${ord}.ª Diferencia Dividida (${idx + 1}.ª resta de ${colCells.length})`
          : `Calculamos la ${ord}.ª Diferencia Dividida`;

        const topSignNote = cell.topVal.isNegative()
          ? ` (como el número de arriba es negativo, menos por menos da más: <span class="math-inline">${renderFracHTML(cell.bottomVal)} + ${renderFracHTML(cell.topVal.abs())}</span>)`
          : '';
        const xTopSignNote = cell.xTop.isNegative()
          ? ` (como <span class="math-inline">x<sub>${cell.idxTop}</sub></span> es negativo, menos por menos da más: <span class="math-inline">${renderFracHTML(cell.xBottom)} + ${renderFracHTML(cell.xTop.abs())}</span>)`
          : '';

        const whyCellHTML = renderWhyBoxHTML(
          '¿Por qué se restaron y dividieron así estos números?',
          `
            <ol class="why-step-list" style="list-style: none; padding-left: 0;">
              <li>
                <strong>1. Arriba de la fracción (numerador):</strong>
                En ${prevColName}, el número de abajo es <span class="math-inline"><strong>${renderFracHTML(cell.bottomVal)}</strong></span> y el de arriba es <span class="math-inline"><strong>${renderFracHTML(cell.topVal)}</strong></span>. Restamos abajo menos arriba:
                <span class="math-inline">${bHTML} ${MINUS} ${tHTML}${topSignNote} = <strong>${renderFracHTML(cell.numDiff)}</strong></span>.
              </li>
              <li>
                <strong>2. Abajo de la fracción (denominador):</strong>
                Siguiendo las diagonales hasta la columna <span class="math-inline">x<sub>i</sub></span>, la <span class="math-inline">x</span> de abajo es <span class="math-inline">x<sub>${cell.idxBottom}</sub> = <strong>${renderFracHTML(cell.xBottom)}</strong></span> y la <span class="math-inline">x</span> de arriba es <span class="math-inline">x<sub>${cell.idxTop}</sub> = <strong>${renderFracHTML(cell.xTop)}</strong></span>. Restamos abajo menos arriba:
                <span class="math-inline">${xbHTML} ${MINUS} ${xtHTML}${xTopSignNote} = <strong>${renderFracHTML(cell.denDiff)}</strong></span>.
              </li>
              <li>
                <strong>3. Dividimos ambos resultados:</strong>
                <span class="math-inline">(${renderFracHTML(cell.numDiff)}) ÷ (${renderFracHTML(cell.denDiff)}) = <strong>${renderFracHTML(cell.val)}</strong></span>.
              </li>
            </ol>
          `
        );

        steps.push({
          title: stepSubtitle,
          html: `
            <p class="step-explanation">
              ${idx === 0
                ? `Empezamos con el primer par de números de ${prevColName}: <strong>el número de abajo (${renderFracHTML(cell.bottomVal)}) se resta con el de arriba (${renderFracHTML(cell.topVal)})</strong>, y en el denominador restamos <strong>x<sub>${cell.idxBottom}</sub> = ${renderFracHTML(cell.xBottom)} con x<sub>${cell.idxTop}</sub> = ${renderFracHTML(cell.xTop)}</strong>:`
                : `Ahora bajamos un lugar en la tabla (observa la flechita <strong>→</strong>): tomamos el siguiente par de ${prevColName} donde <strong>el número de abajo (${renderFracHTML(cell.bottomVal)}) se resta con el de arriba (${renderFracHTML(cell.topVal)})</strong>, y dividimos entre <strong>x<sub>${cell.idxBottom}</sub> = ${renderFracHTML(cell.xBottom)} menos x<sub>${cell.idxTop}</sub> = ${renderFracHTML(cell.xTop)}</strong>:`}
            </p>
            <div class="dd-chain-flow">
              ${progressiveChainWithArrows}
            </div>
            ${whyCellHTML}
            ${buildDividedDiffVisualSVG(workNodes, ddTable, ord, ord, idx, false)}
          `
        });
      }
    }

    // -------------------------------------------------------------------------
    // PASO SIGUIENTE: Tomamos los coeficientes a_0, a_1, ..., a_n de la tabla
    // -------------------------------------------------------------------------
    const coeffs = [];
    for (let ord = 0; ord <= n; ord++) {
      coeffs.push(ddTable[ord][0].val);
    }

    const coeffsBadgesHTML = coeffs.map((c, ord) => `
      <span class="math-inline" style="display: inline-block; margin-right: 1.1rem;">
        <strong>a<sub>${ord}</sub> = ${renderFracHTML(c)}</strong>
      </span>
    `).join('');

    steps.push({
      title: 'Elegimos los coeficientes en la Tabla Vertical',
      html: `
        <p class="step-explanation">
          ¡Ya completamos toda la tabla vertical! Ahora, tal como hace el profesor en la pizarra, <strong>encerramos el primer valor de cada columna (la fila de hasta arriba)</strong>. Esos son nuestros coeficientes <span class="math-inline">a<sub>0</sub>, a<sub>1</sub>, …, a<sub>${n}</sub></span>:
        </p>
        ${buildDividedDiffVisualSVG(workNodes, ddTable, n, null, null, true)}
        ${buildDividedDiffHTMLTable(workNodes, ddTable, n, true)}
        <div class="math-block">
          ${coeffsBadgesHTML}
        </div>
      `
    });

    // -------------------------------------------------------------------------
    // PASO SIGUIENTE: Reemplazamos en P(x) y desarrollamos cada término
    // -------------------------------------------------------------------------
    const substitutedTerms = [];
    const termPolys = [];
    const termDevelopmentLines = [];

    for (let k = 0; k <= n; k++) {
      const ak = coeffs[k];
      if (k === 0) {
        const akBadge0 = `<span class="coeff-highlight" title="Coeficiente a₀ tomado de la tablita">${renderFracHTML(ak, { parensIfNeg: true })}</span>`;
        substitutedTerms.push(akBadge0);
        termPolys.push([ak]);
        const whyTerm0 = renderWhyBoxHTML(
          '¿Por qué el Término 0 queda así?',
          `El primer término de la fórmula de Newton es simplemente <span class="math-inline">a<sub>0</sub></span> (sin factores con <span class="math-inline">x</span>), que corresponde al primer valor de la columna <span class="math-inline">y<sub>i</sub></span>: <span class="math-inline"><strong>a<sub>0</sub> = ${renderFracHTML(ak)}</strong></span>.`
        );
        termDevelopmentLines.push(`
          <div class="math-block">
            <strong>Término 0:</strong> &nbsp; a<sub>0</sub> = ${akBadge0}
          </div>
          ${whyTerm0}
        `);
      } else {
        const factorNodes = [];
        const factorIndices = [];
        for (let j = 0; j < k; j++) {
          factorNodes.push(workNodes[j].x);
          factorIndices.push(j);
        }
        const factorsHTML = factorNodes.map(xj => renderNewtonFactorHTML(xj)).join('');
        const factorsColoredHTML = factorNodes.map(xj => renderNewtonFactorHTML(xj, true)).join('');
        const akParen = renderFracHTML(ak, { parensIfNeg: true });
        const akBadge = `<span class="coeff-highlight" title="Coeficiente a${k} tomado de la tablita">${akParen}</span>`;
        substitutedTerms.push(`${akBadge}${factorsColoredHTML}`);

        // Multiplicar los factores (x - x_0)(x - x_1)...
        let monicPoly = [new Frac(1n)];
        for (const xj of factorNodes) {
          monicPoly = polyMul(monicPoly, [xj.neg(), new Frac(1n)]);
        }
        const scaledTermPoly = polyScale(monicPoly, ak);
        termPolys.push(scaledTermPoly);

        // Cadena de desarrollo igual que el profesor (con math-eq-stage para salto de línea limpio sin scroll horizontal)
        const chain = [];
        const rawChain = [];
        function pushChain(sRaw, sStyled) {
          if (rawChain.length === 0 || rawChain[rawChain.length - 1] !== sRaw) {
            rawChain.push(sRaw);
            chain.push(`<span class="math-eq-stage">${sStyled || sRaw}</span>`);
          }
        }

        pushChain(`${akParen}${factorsHTML}`, `${akBadge}${factorsColoredHTML}`);
        if (factorNodes.length >= 2) {
          const unsimp = renderUnsimplifiedTwoFactorsHTML(factorNodes[0], factorNodes[1]);
          const rem = factorNodes.slice(2).map(xj => renderNewtonFactorHTML(xj)).join('');
          pushChain(`${akParen}(${unsimp})${rem}`);
          pushChain(`${akParen}(${renderPolyHTML(monicPoly)})`);
        }
        pushChain(renderPolyHTML(scaledTermPoly));

        const whyTermK = renderWhyBoxHTML(
          `¿Por qué da este resultado el Término ${k}? Ver paso a paso`,
          buildFactorMultiplicationBreakdownHTML(factorNodes, factorIndices, ak, `a<sub>${k}</sub>`, true)
        );

        termDevelopmentLines.push(`
          <div class="math-block">
            <span class="math-eq-stage"><strong>Término ${k}:</strong> &nbsp; a<sub>${k}</sub>${ factorNodes.map((_, j) => `(x ${MINUS} x<sub>${j}</sub>)`).join('') }</span> = ${chain.join(' = ')}
          </div>
          ${whyTermK}
        `);
      }
    }

    // Explicación (?) de la sustitución general en P(x)
    const substExplanationBullets = coeffs.map((ak, k) => {
      if (k === 0) {
        return `<li><strong>Para <span class="math-inline">a<sub>0</sub></span>:</strong> reemplazamos <span class="math-inline">a<sub>0</sub> = <strong>${renderFracHTML(ak)}</strong></span>.</li>`;
      }
      const fList = [];
      for (let j = 0; j < k; j++) {
        const xj = workNodes[j].x;
        fList.push(`<span class="math-inline">x<sub>${j}</sub> = ${renderFracHTML(xj)} → <strong>${renderNewtonFactorHTML(xj)}</strong></span>`);
      }
      return `<li><strong>Para <span class="math-inline">a<sub>${k}</sub>${Array.from({ length: k }, (_, j) => `(x ${MINUS} x<sub>${j}</sub>)`).join('')}</span>:</strong> reemplazamos <span class="math-inline">a<sub>${k}</sub> = <strong>${renderFracHTML(ak, { parensIfNeg: true })}</strong></span>${ak.isNegative() ? ' (entre paréntesis por ser negativo)' : ' (sin paréntesis por ser positivo)'} y en los factores sustituimos ${fList.join(', ')}.</li>`;
    }).join('');

    const whyGeneralSubstitution = renderWhyBoxHTML(
      '¿Por qué quedó así la sustitución en P(x)?',
      `
        <div>Sustituimos en <span class="math-inline">P(x) = ${workNodes.map((_, k) => k === 0 ? 'a<sub>0</sub>' : `a<sub>${k}</sub>${Array.from({ length: k }, (__, j) => `(x ${MINUS} x<sub>${j}</sub>)`).join('')}`).join(' + ')}</span> cada coeficiente y cada nodo:</div>
        <ul class="why-sublist">${substExplanationBullets}</ul>
      `
    );

    // Sumar todos los términos para obtener P(x)
    let finalPoly = [new Frac(0n)];
    for (const tp of termPolys) {
      finalPoly = polyAdd(finalPoly, tp);
    }
    const finalPolyHTML = renderPolyHTML(finalPoly);

    // Agrupación por grados
    const groupedByDegreeLines = [];
    const ddSourceLabels = termPolys.map((_, k) => `Término ${k}`);
    for (let deg = n; deg >= 0; deg--) {
      const coeffsAtDeg = termPolys.map(tp => (deg < tp.length ? tp[deg] : new Frac(0n)));
      const nonZero = coeffsAtDeg.filter(c => !c.isZero());
      const sumExpr = nonZero.length > 0
        ? nonZero.map((c, idx) => renderTermWithSignHTML(c, 0, idx === 0)).join('')
        : '0';
      const needsParens = deg > 0 && (nonZero.length > 1 || (nonZero.length === 1 && nonZero[0].isNegative()));
      const wrappedSumExpr = needsParens ? `(${sumExpr})` : sumExpr;
      const totalAtDeg = deg < finalPoly.length ? finalPoly[deg] : new Frac(0n);
      const varLabel = deg === 0 ? '' : (deg === 1 ? 'x' : `x<sup>${deg}</sup>`);
      const degTitle = deg === 0
        ? 'Término independiente'
        : (deg === 1 ? 'Términos con x' : `Términos con x<sup>${deg}</sup>`);

      const whyDeg = renderWhyBoxHTML(
        `¿De dónde salen estos números y cómo se suman? (${degTitle})`,
        buildDegreeGroupingBreakdownHTML(deg, termPolys, ddSourceLabels, totalAtDeg)
      );

      groupedByDegreeLines.push(`
        <div class="math-block">
          <span style="font-style: normal; font-weight: 600; color: var(--ink-secondary);">${degTitle}:</span>
          <span class="math-eq-stage">${wrappedSumExpr}${varLabel}</span> = <span class="math-eq-stage">${renderTermWithSignHTML(totalAtDeg, deg, true)}</span>
        </div>
        ${whyDeg}
      `);
    }

    steps.push({
      title: 'Sustituimos en P(x), desarrollamos y sumamos',
      html: `
        <p class="step-explanation">
          Reemplazamos los coeficientes <span class="math-inline">a<sub>0</sub>, …, a<sub>${n}</sub></span> y los nodos <span class="math-inline">x<sub>i</sub></span> en la fórmula de Newton:
        </p>
        <div class="math-block">
          P(x) = ${substitutedTerms.join(' + ')}
        </div>
        ${whyGeneralSubstitution}
        <p class="step-explanation">
          Desarrollamos cada término paso a paso:
        </p>
        ${termDevelopmentLines.join('')}
        <p class="step-explanation">
          Agrupamos términos semejantes según la potencia de <span class="math-inline">x</span>:
        </p>
        ${groupedByDegreeLines.join('')}
        <p class="step-explanation">
          ¡Y llegamos exactamente al mismo <strong>polinomio interpolador final</strong>!:
        </p>
        <div class="math-block math-final-highlight">
          P(x) = ${finalPolyHTML}
        </div>
      `
    });

    // -------------------------------------------------------------------------
    // PASO FINAL: Verificación en cada nodo y gráfica
    // -------------------------------------------------------------------------
    const verificationItemsHTML = workNodes.map((pt, i) => {
      const xVal = pt.x;
      const evalSubHTML = renderPolyEvaluationSubstitutionHTML(finalPoly, xVal);
      const evaluatedY = polyEval(finalPoly, xVal);
      const whyEval = renderWhyBoxHTML(
        `¿Cómo se calculó P(${xVal.toPlainString()}) paso a paso?`,
        buildPolyEvaluationBreakdownHTML(finalPoly, xVal)
      );

      return `
        <li class="verification-item" style="flex-direction: column; align-items: stretch;">
          <div style="display: flex; align-items: center; justify-content: space-between; gap: 0.75rem; flex-wrap: wrap;">
            <span class="math-inline">
              <strong>Nodo ${i}</strong> (${pt.x.toPlainString()}, ${pt.y.toPlainString()}):
              &nbsp; P(${renderFracHTML(xVal)}) = ${evalSubHTML} = <strong>${renderFracHTML(evaluatedY)}</strong> = y<sub>${i}</sub>
            </span>
            <span class="check-badge" aria-label="Verificado">✓ Cumple</span>
          </div>
          ${whyEval}
        </li>
      `;
    }).join('');

    const svgCurveAndNodes = buildCartesianSVG(
      planeNodes,
      finalPoly,
      'Gráfica del polinomio obtenido por Diferencias Divididas',
      evalPoint
    );

    steps.push({
      title: 'Verificamos con la gráfica',
      html: `
        <p class="step-explanation">
          Comprobamos que <span class="math-inline">P(x<sub>i</sub>) = y<sub>i</sub></span> se cumple en cada nodo con el que trabajamos:
        </p>
        <ul class="verification-list">
          ${verificationItemsHTML}
        </ul>
        <p class="step-explanation">
          Y verificamos en el plano cartesiano cómo nuestra curva pasa por todos los ${N} puntos de trabajo:
        </p>
        ${svgCurveAndNodes}
      `
    });

    return steps;
  }

  // ===========================================================================
  // Utilidades DOM compartidas (Barra de salto de pasos, Hover en tablita, Gráfica interactiva, Modo Práctica, etc.)
  // ===========================================================================
  let openAllWhyExplanations = false;
  let practiceModeActive = false;
  let fullBoardActive = false;
  let stickyScrollSpyInitialized = false;
  let currentJumpHandler = null;
  let currentStepsTotal = 0;

  function prefersReducedMotion() {
    return window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  }

  function updateStickyOffsets() {
    if (typeof document === 'undefined' || !document.documentElement) return;
    const navbar = document.querySelector('.top-navbar');
    if (navbar && navbar.offsetHeight) {
      document.documentElement.style.setProperty('--navbar-height', `${navbar.offsetHeight}px`);
    }
    const jumper = document.querySelector('.step-jumper-bar');
    if (jumper && !jumper.hidden && jumper.offsetHeight) {
      document.documentElement.style.setProperty('--jumper-height', `${jumper.offsetHeight}px`);
    }
  }

  function scrollToElement(el) {
    if (!el) return;
    updateStickyOffsets();
    if (el.classList && el.classList.contains('step-card')) {
      if (!el.hasAttribute('tabindex')) {
        el.setAttribute('tabindex', '-1');
      }
      if (typeof el.focus === 'function') {
        try {
          el.focus({ preventScroll: true });
        } catch (_) {}
      }
    }
    el.scrollIntoView({
      behavior: prefersReducedMotion() ? 'auto' : 'smooth',
      block: 'start'
    });
  }

  function getShortStepTag(cleanTitle, idx) {
    if (idx === 0) return 'Nodos';
    if (/Armamos la Tabla/i.test(cleanTitle)) return 'Tablita';
    if (/Elegimos los coeficientes/i.test(cleanTitle)) return 'Coef.';
    if (/Sustituimos en P\(x\)|Multiplicamos cada/i.test(cleanTitle)) return 'P(x)';
    if (/Verificamos/i.test(cleanTitle)) return 'Gráfica';
    const lMatch = cleanTitle.match(/Calculamos\s+L(\d+)/i);
    if (lMatch) return `L${lMatch[1]}`;
    return '';
  }

  function getActiveStepIndex() {
    if (typeof document === 'undefined') return 0;
    const cards = Array.from(document.querySelectorAll('.step-card'));
    if (cards.length === 0) return 0;

    const navbar = document.querySelector('.top-navbar');
    const jumper = document.querySelector('.step-jumper-bar');
    const offsetTop = (navbar ? navbar.offsetHeight : 64) + (jumper && !jumper.hidden ? jumper.offsetHeight : 48) + 45;

    let activeIdx = 0;
    for (let i = 0; i < cards.length; i++) {
      const rect = cards[i].getBoundingClientRect();
      if (rect.top <= offsetTop) {
        const stepIdx = parseInt(cards[i].getAttribute('data-step-index'), 10);
        if (!isNaN(stepIdx)) activeIdx = stepIdx;
      }
    }
    return activeIdx;
  }

  function updateCurrentStepHighlight() {
    if (typeof document === 'undefined') return;
    const activeIdx = getActiveStepIndex();
    document.querySelectorAll('.step-pill-btn[data-jump-idx]').forEach(btn => {
      const bIdx = parseInt(btn.getAttribute('data-jump-idx'), 10);
      btn.classList.toggle('is-current', bIdx === activeIdx);
    });
    const prevBtn = document.querySelector('.js-jumper-prev-btn');
    const nextBtn = document.querySelector('.js-jumper-next-btn');
    if (prevBtn) prevBtn.disabled = activeIdx <= 0;
    if (nextBtn) nextBtn.disabled = currentStepsTotal > 0 && activeIdx >= currentStepsTotal - 1;
  }

  function initStickyScrollSpy() {
    if (stickyScrollSpyInitialized || typeof window === 'undefined' || !window.addEventListener) return;
    stickyScrollSpyInitialized = true;
    window.addEventListener('resize', () => {
      updateStickyOffsets();
      updateCurrentStepHighlight();
    });
    window.addEventListener('scroll', () => {
      updateCurrentStepHighlight();
    }, { passive: true });

    // Navegación con flechas del teclado (← y →) cuando el usuario no está escribiendo en un campo de texto
    document.addEventListener('keydown', (e) => {
      if (e.altKey || e.ctrlKey || e.metaKey || e.shiftKey) return;
      const tag = e.target && e.target.tagName ? e.target.tagName.toLowerCase() : '';
      if (tag === 'input' || tag === 'textarea' || tag === 'select' || (e.target && e.target.isContentEditable)) {
        return;
      }
      if (typeof currentJumpHandler !== 'function' || currentStepsTotal <= 0) return;

      if (e.key === 'ArrowRight') {
        const activeIdx = getActiveStepIndex();
        if (activeIdx < currentStepsTotal - 1) {
          e.preventDefault();
          currentJumpHandler(activeIdx + 1);
        }
      } else if (e.key === 'ArrowLeft') {
        const activeIdx = getActiveStepIndex();
        if (activeIdx > 0) {
          e.preventDefault();
          currentJumpHandler(activeIdx - 1);
        }
      }
    });
  }

  // Convierte el contenido de un bloque matemático final en texto limpio para portapapeles
  function extractCleanMathText(el) {
    if (!el) return '';
    const clone = el.cloneNode(true);
    clone.querySelectorAll('.copy-poly-btn').forEach(b => b.remove());
    clone.querySelectorAll('sup').forEach(sup => {
      sup.textContent = '^' + sup.textContent;
    });
    clone.querySelectorAll('sub').forEach(sub => {
      sub.textContent = '_' + sub.textContent;
    });
    return clone.textContent.replace(/\s+/g, ' ').trim();
  }

  function enhanceFinalHighlightCopyButtons(rootEl) {
    if (!rootEl || !rootEl.querySelectorAll) return;
    const boxes = rootEl.classList && rootEl.classList.contains('math-final-highlight')
      ? [rootEl]
      : Array.from(rootEl.querySelectorAll('.math-final-highlight'));

    boxes.forEach(box => {
      if (box.querySelector('.copy-poly-btn')) return;
      if (/P\(x\)\s*=\s*\.\.\./.test(box.textContent)) return;
      box.classList.add('has-copy-btn');
      if (!box.querySelector('.math-final-expr')) {
        const exprWrap = document.createElement('span');
        exprWrap.className = 'math-final-expr';
        while (box.firstChild) {
          exprWrap.appendChild(box.firstChild);
        }
        box.appendChild(exprWrap);
      }
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'copy-poly-btn';
      btn.title = 'Copiar polinomio al portapapeles';
      btn.innerHTML = '📋 Copiar';
      btn.addEventListener('click', (e) => {
        e.stopPropagation();
        const text = extractCleanMathText(box);
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text).then(() => {
            btn.innerHTML = '✓ ¡Copiado!';
            btn.classList.add('is-copied');
            setTimeout(() => {
              btn.innerHTML = '📋 Copiar';
              btn.classList.remove('is-copied');
            }, 1800);
          }).catch(() => {});
        }
      });
      box.appendChild(btn);
    });
  }

  function createStepCardElement(stepData, index, total) {
    const section = document.createElement('section');
    section.className = 'notebook-card step-card';
    section.setAttribute('data-step-index', index);
    section.setAttribute('tabindex', '-1');
    section.innerHTML = `
      <div class="step-header">
        <h2 class="step-title">Paso ${index + 1}: ${stepData.title}</h2>
        <span class="step-badge">Paso ${index + 1} de ${total}</span>
      </div>
      <div class="step-body">
        ${stepData.html}
      </div>
    `;
    if (openAllWhyExplanations) {
      section.querySelectorAll('details.why-box').forEach(d => {
        d.open = true;
      });
    }
    enhanceFinalHighlightCopyButtons(section);
    return section;
  }

  // Barra interactiva siempre visible (fija bajo el menú superior en la pizarra)
  function syncStepJumperBar(stepsContainer, stepsData, revealedCount, onJumpToStep) {
    if (!stepsContainer || !stepsContainer.parentElement) return;
    initStickyScrollSpy();
    updateStickyOffsets();

    currentJumpHandler = onJumpToStep;
    currentStepsTotal = stepsData ? stepsData.length : 0;

    // Mejorar también el cuadro de P(x) lateral en las calculadoras si existe
    document.querySelectorAll('#active-poly-box, #dd-active-poly-box').forEach(box => {
      enhanceFinalHighlightCopyButtons(box);
    });

    const board = stepsContainer.parentElement;
    board.classList.toggle('practice-mode-active', practiceModeActive);

    const layout = document.querySelector('.workspace-layout');
    if (layout) {
      layout.classList.toggle('is-full-board', fullBoardActive);
    }

    let bar = board.querySelector('.step-jumper-bar');

    if (!stepsData || stepsData.length === 0) {
      if (bar) bar.hidden = true;
      return;
    }

    if (!bar) {
      bar = document.createElement('div');
      bar.className = 'step-jumper-bar';
      bar.setAttribute('role', 'navigation');
      bar.setAttribute('aria-label', 'Navegación rápida de pasos de la pizarra');
      board.insertBefore(bar, stepsContainer);
    }

    bar.hidden = false;
    const pillsHTML = stepsData.map((st, idx) => {
      const cleanTitle = st.title.replace(/<[^>]+>/g, '');
      const isRev = idx < revealedCount;
      const shortTag = getShortStepTag(cleanTitle, idx);
      const tagSpan = shortTag ? ` <span class="pill-short-tag">${shortTag}</span>` : '';
      return `
        <button type="button"
                class="step-pill-btn ${isRev ? 'is-revealed' : ''}"
                data-jump-idx="${idx}"
                title="Ir al Paso ${idx + 1}: ${cleanTitle}">
          <span>${idx + 1}</span>${tagSpan}
        </button>
      `;
    }).join('');

    const progressPct = Math.min(100, Math.round((revealedCount / stepsData.length) * 100));

    bar.innerHTML = `
      <div class="step-jumper-left">
        <div class="step-nav-arrows" role="group" aria-label="Paso anterior o siguiente (también puedes usar las flechas ← y → del teclado)">
          <button type="button" class="step-arrow-btn js-jumper-prev-btn" title="Paso anterior (Tecla ←)">‹ Ant.</button>
          <button type="button" class="step-arrow-btn js-jumper-next-btn" title="Siguiente paso (Tecla →)">Sig. ›</button>
        </div>
        ${pillsHTML}
      </div>
      <div class="step-jumper-right">
        <button type="button" class="why-toggle-all-btn js-practice-mode-btn ${practiceModeActive ? 'is-active' : ''}" title="Oculta temporalmente los resultados para que intentes calcularlos tú primero y hagas clic para comprobar">
          <span>🎯 ${practiceModeActive ? 'Salir práctica' : 'Práctica'}</span>
        </button>
        <button type="button" class="why-toggle-all-btn js-why-toggle-btn ${openAllWhyExplanations ? 'is-active' : ''}" title="Abrir u ocultar todas las explicaciones paso a paso">
          <span class="why-icon" aria-hidden="true">?</span>
          <span>${openAllWhyExplanations ? 'Ocultar' : 'Explicaciones'}</span>
        </button>
        <button type="button" class="why-toggle-all-btn js-full-board-btn ${fullBoardActive ? 'is-active' : ''}" title="Ocultar o mostrar el panel izquierdo para ver la pizarra a pantalla completa">
          <span>${fullBoardActive ? '◨ Panel' : '◧ Expandir'}</span>
        </button>
      </div>
      <div class="step-jumper-progress-track" aria-hidden="true">
        <div class="step-jumper-progress-fill" style="width: ${progressPct}%;"></div>
      </div>
    `;

    bar.querySelectorAll('.step-pill-btn').forEach(btn => {
      btn.addEventListener('click', () => {
        const targetIdx = parseInt(btn.getAttribute('data-jump-idx'), 10);
        if (typeof onJumpToStep === 'function') {
          onJumpToStep(targetIdx);
        }
      });
    });

    const prevBtn = bar.querySelector('.js-jumper-prev-btn');
    const nextBtn = bar.querySelector('.js-jumper-next-btn');
    if (prevBtn) {
      prevBtn.addEventListener('click', () => {
        const activeIdx = getActiveStepIndex();
        if (activeIdx > 0 && typeof onJumpToStep === 'function') {
          onJumpToStep(activeIdx - 1);
        }
      });
    }
    if (nextBtn) {
      nextBtn.addEventListener('click', () => {
        const activeIdx = getActiveStepIndex();
        if (activeIdx < stepsData.length - 1 && typeof onJumpToStep === 'function') {
          onJumpToStep(activeIdx + 1);
        }
      });
    }

    const practiceBtn = bar.querySelector('.js-practice-mode-btn');
    if (practiceBtn) {
      practiceBtn.addEventListener('click', () => {
        practiceModeActive = !practiceModeActive;
        if (!practiceModeActive) {
          stepsContainer.querySelectorAll('.is-revealed-answer').forEach(el => el.classList.remove('is-revealed-answer'));
        }
        syncStepJumperBar(stepsContainer, stepsData, revealedCount, onJumpToStep);
      });
    }

    const toggleAllBtn = bar.querySelector('.js-why-toggle-btn');
    if (toggleAllBtn) {
      toggleAllBtn.addEventListener('click', () => {
        openAllWhyExplanations = !openAllWhyExplanations;
        stepsContainer.querySelectorAll('details.why-box').forEach(d => {
          d.open = openAllWhyExplanations;
        });
        syncStepJumperBar(stepsContainer, stepsData, revealedCount, onJumpToStep);
      });
    }

    const fullBoardBtn = bar.querySelector('.js-full-board-btn');
    if (fullBoardBtn) {
      fullBoardBtn.addEventListener('click', () => {
        fullBoardActive = !fullBoardActive;
        syncStepJumperBar(stepsContainer, stepsData, revealedCount, onJumpToStep);
      });
    }

    updateStickyOffsets();
    updateCurrentStepHighlight();
  }

  // Interacción en Modo Práctica (clic para revelar resultados difuminados) y Gráfica Interactiva
  let globalEnhancementsInitialized = false;
  function initGlobalBoardEnhancements() {
    if (globalEnhancementsInitialized || typeof document === 'undefined' || !document.addEventListener) return;
    globalEnhancementsInitialized = true;

    // 1. Clic para revelar respuesta en Modo Práctica
    document.addEventListener('click', (e) => {
      if (!practiceModeActive || !e.target || !e.target.closest) return;
      const target = e.target.closest(
        '.workspace-board.practice-mode-active .math-final-highlight, ' +
        '.workspace-board.practice-mode-active .dd-chain-step strong, ' +
        '.workspace-board.practice-mode-active .math-eq-stage:last-child, ' +
        '.workspace-board.practice-mode-active .verification-item strong'
      );
      if (target) {
        target.classList.toggle('is-revealed-answer');
      }
    });

    // 2. Explorador interactivo con Pointer Events + requestAnimationFrame (60 FPS en escritorio y soporte táctil en móvil/tablet)
    let rafPending = false;
    let latestPointerEvent = null;

    function renderPlotPointerFrame(e, isClick = false) {
      if (!e || !e.target || !e.target.closest) return;
      const svg = e.target.closest('.svg-plot[data-poly-coeffs]');
      if (!svg) return;

      const hoverGroup = svg.querySelector('.svg-hover-group');
      if (!hoverGroup) return;

      const coeffsRaw = svg.getAttribute('data-poly-coeffs');
      if (!coeffsRaw) return;
      const coeffs = coeffsRaw.split(',').map(Number);

      const xMin = parseFloat(svg.getAttribute('data-xmin'));
      const xMax = parseFloat(svg.getAttribute('data-xmax'));
      const yMin = parseFloat(svg.getAttribute('data-ymin'));
      const yMax = parseFloat(svg.getAttribute('data-ymax'));
      const padLeft = parseFloat(svg.getAttribute('data-pad-left'));
      const padRight = parseFloat(svg.getAttribute('data-pad-right'));
      const padTop = parseFloat(svg.getAttribute('data-pad-top'));
      const padBottom = parseFloat(svg.getAttribute('data-pad-bottom'));
      const width = parseFloat(svg.getAttribute('data-width'));
      const height = parseFloat(svg.getAttribute('data-height'));

      const rect = svg.getBoundingClientRect();
      if (!rect.width || !rect.height) return;

      const svgX = ((e.clientX - rect.left) / rect.width) * width;
      const plotW = width - padLeft - padRight;
      const plotH = height - padTop - padBottom;

      if (svgX < padLeft || svgX > width - padRight) {
        hoverGroup.style.display = 'none';
        return;
      }

      const mathX = xMin + ((svgX - padLeft) / plotW) * (xMax - xMin);
      let mathY = 0;
      for (let d = coeffs.length - 1; d >= 0; d--) {
        mathY = mathY * mathX + coeffs[d];
      }

      if (!isFinite(mathY)) {
        hoverGroup.style.display = 'none';
        return;
      }

      const clampedY = Math.max(yMin, Math.min(yMax, mathY));
      const svgY = padTop + ((yMax - clampedY) / (yMax - yMin)) * plotH;

      const vline = hoverGroup.querySelector('.svg-hover-vline');
      const hline = hoverGroup.querySelector('.svg-hover-hline');
      const dot = hoverGroup.querySelector('.svg-hover-dot');
      const badge = hoverGroup.querySelector('.svg-hover-badge');
      const badgeText = hoverGroup.querySelector('.svg-hover-badge-text');
      const badgeBg = hoverGroup.querySelector('.svg-hover-badge-bg');

      if (vline) {
        vline.setAttribute('x1', svgX.toFixed(2));
        vline.setAttribute('x2', svgX.toFixed(2));
      }
      if (hline) {
        hline.setAttribute('y1', svgY.toFixed(2));
        hline.setAttribute('y2', svgY.toFixed(2));
      }
      if (dot) {
        dot.setAttribute('cx', svgX.toFixed(2));
        dot.setAttribute('cy', svgY.toFixed(2));
      }

      const fmtNum = (n) => {
        const rounded = Math.abs(n) < 1e-9 ? 0 : Number(n.toFixed(2));
        return rounded < 0 ? MINUS + Math.abs(rounded).toString() : rounded.toString();
      };
      const labelStr = `(${fmtNum(mathX)}, ${fmtNum(mathY)})`;
      if (badgeText && badge && badgeBg) {
        badgeText.textContent = labelStr;
        const bw = Math.max(116, labelStr.length * 8.2 + 22);
        badgeBg.setAttribute('width', bw.toFixed(1));
        badgeText.setAttribute('x', (bw / 2).toFixed(1));
        const bx = Math.max(padLeft + 4, Math.min(width - padRight - bw - 4, svgX - bw / 2));
        const by = svgY > padTop + 42 ? svgY - 36 : svgY + 12;
        badge.setAttribute('transform', `translate(${bx.toFixed(1)}, ${by.toFixed(1)})`);
      }

      hoverGroup.style.display = 'block';

      if (isClick) {
        const evalInput = document.getElementById('eval-x-input') || document.getElementById('dd-eval-x-input');
        const evalBtn = document.getElementById('eval-x-btn') || document.getElementById('dd-eval-x-btn');
        if (evalInput && evalBtn) {
          const snappedX = Math.abs(mathX - Math.round(mathX * 4) / 4) < 0.04
            ? Math.round(mathX * 4) / 4
            : Number(mathX.toFixed(2));
          evalInput.value = String(snappedX);
          evalBtn.click();
        }
      }
    }

    document.addEventListener('pointermove', (e) => {
      if (!e.target || !e.target.closest) return;
      const svg = e.target.closest('.svg-plot[data-poly-coeffs]');
      if (svg) {
        latestPointerEvent = e;
        if (!rafPending) {
          rafPending = true;
          const schedule = typeof window.requestAnimationFrame === 'function'
            ? window.requestAnimationFrame
            : (cb) => setTimeout(cb, 16);
          schedule(() => {
            rafPending = false;
            if (latestPointerEvent) renderPlotPointerFrame(latestPointerEvent, false);
          });
        }
      } else {
        document.querySelectorAll('.svg-hover-group').forEach(g => {
          g.style.display = 'none';
        });
      }
    }, { passive: true });

    document.addEventListener('pointerdown', (e) => {
      if (!e.target || !e.target.closest) return;
      const svg = e.target.closest('.svg-plot[data-poly-coeffs]');
      if (svg && e.pointerType === 'touch') {
        renderPlotPointerFrame(e, false);
      }
    }, { passive: true });

    document.addEventListener('click', (e) => {
      if (!e.target || !e.target.closest) return;
      const svg = e.target.closest('.svg-plot[data-poly-coeffs]');
      if (svg) {
        renderPlotPointerFrame(e, true);
      }
    });
  }

  // Resaltado interactivo al pasar el puntero o tocar cualquier celda de la tablita (SVG o HTML)
  let tablitaHoverInitialized = false;
  function initInteractiveTablitaHover() {
    initGlobalBoardEnhancements();
    if (tablitaHoverInitialized || typeof document === 'undefined' || !document.addEventListener) return;
    tablitaHoverInitialized = true;

    function clearAllHighlights(rootContainer) {
      if (!rootContainer) return;
      rootContainer.querySelectorAll('.dd-hover-focus').forEach(el => {
        el.classList.remove('dd-hover-focus');
      });
    }

    function highlightCellAndParents(targetEl) {
      const ord = parseInt(targetEl.getAttribute('data-dd-ord'), 10);
      const row = parseInt(targetEl.getAttribute('data-dd-row'), 10);
      const stepCard = targetEl.closest('.step-card') || document;
      clearAllHighlights(stepCard);

      if (isNaN(ord) || isNaN(row) || ord < 1) return;

      stepCard.querySelectorAll(`[data-dd-ord="${ord}"][data-dd-row="${row}"]`).forEach(el => el.classList.add('dd-hover-focus'));
      stepCard.querySelectorAll(`[data-dd-ord="${ord - 1}"][data-dd-row="${row}"], [data-dd-ord="${ord - 1}"][data-dd-row="${row + 1}"]`).forEach(el => el.classList.add('dd-hover-focus'));
      stepCard.querySelectorAll(`[data-dd-x="${row}"], [data-dd-x="${row + ord}"]`).forEach(el => el.classList.add('dd-hover-focus'));
    }

    document.addEventListener('pointerover', (e) => {
      if (!e.target || !e.target.closest) return;
      const cell = e.target.closest('[data-dd-ord]');
      if (cell) {
        highlightCellAndParents(cell);
      }
    });

    document.addEventListener('pointerout', (e) => {
      if (!e.target || !e.target.closest) return;
      const cell = e.target.closest('[data-dd-ord]');
      if (cell) {
        const stepCard = cell.closest('.step-card') || document;
        clearAllHighlights(stepCard);
      }
    });
  }

  // Validación reactiva en tiempo real por campo (marca con .is-invalid / aria-invalid las celdas con sintaxis inválida o x_i duplicados)
  function validateCalculatorRowsRealtime(tbody, evalXInput = null, errorBox = null) {
    if (!tbody) return { valid: true };
    const rows = Array.from(tbody.querySelectorAll('tr'));
    let firstErrorMsg = '';
    const seenXMap = new Map();

    rows.forEach((tr, idx) => {
      const ix = tr.querySelector('.input-x');
      const iy = tr.querySelector('.input-y');
      if (ix) {
        const rawX = ix.value.trim();
        if (rawX === '') {
          ix.classList.remove('is-invalid');
          ix.removeAttribute('aria-invalid');
        } else {
          const rx = parseInputToFrac(rawX);
          if (rx.error) {
            ix.classList.add('is-invalid');
            ix.setAttribute('aria-invalid', 'true');
            if (!firstErrorMsg) firstErrorMsg = `Fila ${idx + 1} (x): ${rx.error}`;
          } else {
            ix.classList.remove('is-invalid');
            ix.removeAttribute('aria-invalid');
            const key = rx.value.toPlainString();
            if (!seenXMap.has(key)) seenXMap.set(key, []);
            seenXMap.get(key).push({ input: ix, rowNum: idx + 1 });
          }
        }
      }
      if (iy) {
        const rawY = iy.value.trim();
        if (rawY === '') {
          iy.classList.remove('is-invalid');
          iy.removeAttribute('aria-invalid');
        } else {
          const ry = parseInputToFrac(rawY);
          if (ry.error) {
            iy.classList.add('is-invalid');
            iy.setAttribute('aria-invalid', 'true');
            if (!firstErrorMsg) firstErrorMsg = `Fila ${idx + 1} (y): ${ry.error}`;
          } else {
            iy.classList.remove('is-invalid');
            iy.removeAttribute('aria-invalid');
          }
        }
      }
    });

    seenXMap.forEach((entries, xKey) => {
      if (entries.length > 1) {
        entries.forEach(item => {
          item.input.classList.add('is-invalid');
          item.input.setAttribute('aria-invalid', 'true');
        });
        if (!firstErrorMsg) {
          const rowNums = entries.map(e => e.rowNum).join(' y ');
          firstErrorMsg = `Las filas ${rowNums} tienen la misma coordenada x (${xKey}). Cada nodo debe tener una x distinta.`;
        }
      }
    });

    if (evalXInput) {
      const rawEval = evalXInput.value.trim();
      if (rawEval !== '') {
        const rEval = parseInputToFrac(rawEval);
        if (rEval.error) {
          evalXInput.classList.add('is-invalid');
          evalXInput.setAttribute('aria-invalid', 'true');
        } else {
          evalXInput.classList.remove('is-invalid');
          evalXInput.removeAttribute('aria-invalid');
        }
      } else {
        evalXInput.classList.remove('is-invalid');
        evalXInput.removeAttribute('aria-invalid');
      }
    }

    if (errorBox) {
      if (firstErrorMsg) {
        errorBox.textContent = firstErrorMsg;
        errorBox.hidden = false;
      } else {
        errorBox.textContent = '';
        errorBox.hidden = true;
      }
    }

    return { valid: !firstErrorMsg, error: firstErrorMsg };
  }

  // Obtiene el tema activo ('light' o 'dark') si el usuario lo fijó explícitamente
  function getExplicitTheme() {
    if (typeof document === 'undefined' || !document.documentElement) return null;
    const t = document.documentElement.getAttribute('data-theme');
    return (t === 'light' || t === 'dark') ? t : null;
  }

  // Sincronización de nodos, casillas seleccionadas, punto x evaluado y tema en la URL (sin backend ni localStorage)
  function buildNodesQueryString(tbody, workCount = null, evalXStr = null) {
    if (!tbody) return '';
    const rows = Array.from(tbody.querySelectorAll('tr'));
    const pairs = [];
    const activeFlags = [];
    rows.forEach(tr => {
      const ix = tr.querySelector('.input-x');
      const iy = tr.querySelector('.input-y');
      const chk = tr.querySelector('.node-use-check');
      if (ix && iy && ix.value.trim() !== '' && iy.value.trim() !== '') {
        pairs.push(`${ix.value.trim()},${iy.value.trim()}`);
        activeFlags.push(chk ? (chk.checked ? '1' : '0') : '1');
      }
    });
    if (pairs.length < 2) return '';
    const params = new URLSearchParams();
    params.set('nodos', pairs.join(';'));
    if (workCount !== null && !isNaN(workCount)) {
      params.set('work', String(workCount));
    }
    if (activeFlags.some(f => f === '0')) {
      params.set('sel', activeFlags.join(''));
    }
    if (evalXStr !== null && evalXStr !== undefined && String(evalXStr).trim() !== '') {
      params.set('x', String(evalXStr).trim());
    }
    const theme = getExplicitTheme();
    if (theme) {
      params.set('tema', theme);
    }
    return '?' + params.toString();
  }

  function syncCalculatorURLState(tbody, workCount = null, evalXStr = null) {
    if (typeof window === 'undefined' || !window.history || !window.history.replaceState) return;
    const qs = buildNodesQueryString(tbody, workCount, evalXStr);
    if (qs) {
      try {
        window.history.replaceState(null, '', window.location.pathname + qs);
      } catch (_) {}
    }
  }

  function copyShareableExerciseURL(btn, tbody, workCount = null, evalXStr = null) {
    syncCalculatorURLState(tbody, workCount, evalXStr);
    const fullUrl = window.location.href;
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(fullUrl).then(() => {
        if (!btn) return;
        const originalHTML = btn.getAttribute('data-orig-label') || btn.innerHTML;
        btn.setAttribute('data-orig-label', originalHTML);
        btn.innerHTML = '✓ ¡Enlace del ejercicio copiado!';
        btn.classList.add('is-copied');
        setTimeout(() => {
          btn.innerHTML = originalHTML;
          btn.classList.remove('is-copied');
        }, 2000);
      }).catch(() => {});
    }
  }

  function parseNodesFromURL() {
    if (typeof window === 'undefined' || !window.location || !window.location.search) return null;
    try {
      const params = new URLSearchParams(window.location.search);
      const raw = params.get('nodos');
      if (!raw) return null;
      const selStr = params.get('sel') || '';
      const nodes = raw.split(';').map((pair, idx) => {
        const parts = pair.split(',');
        if (parts.length < 2) return null;
        const active = selStr.length > idx ? selStr[idx] !== '0' : null;
        return {
          x: parts[0].trim(),
          y: parts.slice(1).join(',').trim(),
          active
        };
      }).filter(Boolean);
      if (nodes.length < 2) return null;
      const workParam = parseInt(params.get('work'), 10);
      const evalXParam = params.get('x');
      return {
        nodes,
        workCount: !isNaN(workParam) && workParam >= 2 ? workParam : null,
        evalX: evalXParam !== null && evalXParam.trim() !== '' ? evalXParam.trim() : null
      };
    } catch (_) {
      return null;
    }
  }

  function bindCrossMethodNavigation(tbody, getWorkCountFn = null, getEvalXFn = null) {
    if (typeof document === 'undefined' || !document.querySelectorAll) return;
    document.querySelectorAll('.nav-links a.nav-link, a.js-sync-nodes-link').forEach(link => {
      link.addEventListener('click', (e) => {
        const href = link.getAttribute('href');
        if (!href || href.startsWith('#')) return;
        const baseHref = href.split('?')[0];
        if (!/calculadora/i.test(baseHref)) return;
        const wc = typeof getWorkCountFn === 'function' ? getWorkCountFn() : null;
        const ex = typeof getEvalXFn === 'function' ? getEvalXFn() : null;
        const qs = buildNodesQueryString(tbody, wc, ex);
        if (qs) {
          e.preventDefault();
          window.location.href = baseHref + qs;
        }
      });
    });
  }

  function initThemeToggle(btnId = 'theme-toggle-btn') {
    initInteractiveTablitaHover();
    if (typeof window !== 'undefined' && window.location && window.location.search) {
      try {
        const params = new URLSearchParams(window.location.search);
        const urlTheme = params.get('tema');
        if (urlTheme === 'light' || urlTheme === 'dark') {
          document.documentElement.setAttribute('data-theme', urlTheme);
        }
      } catch (_) {}
    }

    // Propagar el tema elegido al navegar entre páginas
    if (typeof document !== 'undefined' && document.querySelectorAll) {
      document.querySelectorAll('a[href$=".html"]').forEach(link => {
        link.addEventListener('click', (e) => {
          if (e.defaultPrevented) return;
          const theme = getExplicitTheme();
          if (!theme) return;
          const href = link.getAttribute('href');
          if (!href || href.startsWith('#')) return;
          e.preventDefault();
          const [base, existingQs] = href.split('?');
          const params = new URLSearchParams(existingQs || '');
          params.set('tema', theme);
          window.location.href = `${base}?${params.toString()}`;
        });
      });
    }

    const btn = document.getElementById(btnId);
    if (!btn) return;

    function isDarkThemeActive() {
      const currentAttr = document.documentElement.getAttribute('data-theme');
      const systemDark = typeof window !== 'undefined' && window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
      return currentAttr === 'dark' || (!currentAttr && systemDark);
    }

    function syncThemeButtonLabel() {
      btn.innerHTML = isDarkThemeActive() ? '☀️ Tema claro' : '🌙 Tema oscuro';
    }

    syncThemeButtonLabel();

    btn.addEventListener('click', () => {
      const nextTheme = isDarkThemeActive() ? 'light' : 'dark';
      document.documentElement.setAttribute('data-theme', nextTheme);
      syncThemeButtonLabel();

      if (typeof window !== 'undefined' && window.history && window.history.replaceState) {
        try {
          const params = new URLSearchParams(window.location.search);
          params.set('tema', nextTheme);
          window.history.replaceState(null, '', `${window.location.pathname}?${params.toString()}`);
        } catch (_) {}
      }
    });
  }

  global.LagrangeApp = {
    MINUS,
    Frac,
    parseInputToFrac,
    polyEval,
    computeLagrangePolynomial,
    computeDividedDifferencesTable,
    computeDividedDifferencesPolynomial,
    renderFracHTML,
    renderPolyHTML,
    renderPolyEvaluationSubstitutionHTML,
    renderWhyBoxHTML,
    buildPolyEvaluationBreakdownHTML,
    buildCartesianSVG,
    buildDividedDiffVisualSVG,
    buildDividedDiffHTMLTable,
    generateAllStepsData,
    generateDividedDiffStepsData,
    scrollToElement,
    createStepCardElement,
    syncStepJumperBar,
    validateCalculatorRowsRealtime,
    syncCalculatorURLState,
    copyShareableExerciseURL,
    parseNodesFromURL,
    bindCrossMethodNavigation,
    initThemeToggle
  };
})(window);
