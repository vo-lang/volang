// WebKitGTK can expose POSIX "C" as a browser language. Intl consumers expect
// BCP 47 tags. Repair invalid host values before application modules execute.
(() => {
  const valid = language => {
    try { return typeof language === 'string' && Intl.getCanonicalLocales(language).length === 1; }
    catch { return false; }
  };
  const languages = navigator.languages.filter(valid);
  if (valid(navigator.language) && languages.length === navigator.languages.length) return;
  const language = valid(navigator.language) ? navigator.language : languages[0] ?? 'en-US';
  if (!languages.length) languages.push(language);
  Object.defineProperties(navigator, {
    language: { configurable: true, get: () => language },
    languages: { configurable: true, get: () => Object.freeze([...languages]) },
  });
})();
