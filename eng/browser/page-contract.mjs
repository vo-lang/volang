import {expect} from '@playwright/test';

export class PageContract {
  constructor(page) {this.page=page;}
  evaluate(expression) {return this.page.evaluate(expression);}
}

export async function pollEvaluation(contract, expression, predicate, timeout) {
  let value;
  await expect.poll(async()=>{
    value=await contract.evaluate(expression);
    return predicate(value);
  },{timeout,intervals:[25,50,100],message:`page contract: ${expression.slice(0,180)}`}).toBe(true);
  return value;
}
