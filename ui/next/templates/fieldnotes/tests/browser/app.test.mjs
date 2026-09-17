import {test, expect} from './fixtures.mjs';

test('browse a collection and save personal preferences', async ({page, appURL, backend}) => {
  await page.goto(appURL);
  const search = page.getByRole('searchbox', {name:'Search the library'});
  await search.fill('design');
  await expect(page).toHaveURL(/q=design/);
  await expect(page.getByText('6 notes to explore', {exact:true})).toBeVisible();
  await search.fill('');
  await expect(page.getByText('24 notes to explore', {exact:true})).toBeVisible();
  await page.getByRole('navigation', {name:'Pagination'}).getByRole('button', {name:'Next', exact:true}).click();
  await expect(page).toHaveURL(/page=2/);
  await page.getByRole('link', {name:'Keep a field journal', exact:true}).click();
  await expect(page.getByRole('heading', {name:'Keep a field journal', exact:true})).toBeVisible();
  await page.goBack();
  await expect(page).toHaveURL(/page=2/);
  await page.getByRole('link', {name:'Preferences', exact:true}).click();
  await page.getByRole('textbox', {name:'Collection name'}).fill('My reading corner');
  await page.getByRole('combobox', {name:'Reading density'}).selectOption('compact');
  await page.getByRole('button', {name:'Save preferences', exact:true}).click();
  await expect(page.getByText('Your preferences are saved.', {exact:true})).toBeVisible();
  // Keep the selected execution backend when checking a new document.
  const next = new URL(page.url()); next.searchParams.set('backend', backend);
  await page.goto(next.href);
  await expect(page.getByRole('textbox', {name:'Collection name'})).toHaveValue('My reading corner');
  await expect(page.getByRole('combobox', {name:'Reading density'})).toHaveValue('compact');
});
