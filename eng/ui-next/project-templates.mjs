// The catalog also supplies compile-time native CLI help without Node.js.
import catalog from './project-templates.json' with {type:'json'};

function template(name,value) {
  if (!value.example) return {...value,directory:`templates/${name}`};
  const source=`examples/${value.example}`;
  return {...value,files:[
    [`${source}/app.vo`,'app/app.vo'],
    [`${source}/app.css`,'web/app.css'],
    [`${source}/tests/browser/app.test.mjs`,'tests/browser/app.test.mjs'],
  ]};
}
export const projectTemplates=Object.freeze(Object.fromEntries(
  Object.entries(catalog).map(([name,value])=>[name,template(name,value)]),
));
export const templateNames=Object.freeze(Object.keys(projectTemplates));
export const deliveredExampleFiles=Object.freeze(
  Object.values(projectTemplates).flatMap(value=>value.files?.map(([source])=>source)??[]),
);
