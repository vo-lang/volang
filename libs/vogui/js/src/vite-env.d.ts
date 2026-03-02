/// <reference types="vite/client" />

// Vite ?inline CSS import — returns the CSS content as a string.
declare module '*.css?inline' {
    const content: string;
    export default content;
}
