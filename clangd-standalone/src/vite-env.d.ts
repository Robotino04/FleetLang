/// <reference types="vite/client" />

declare module '*?worker&url' {
    const content: string;
    export default content;
}

declare module '*?raw' {
    const content: string;
    export default content;
}

interface ImportMeta {
    glob(pattern: string, options?: { query?: string }): Record<string, () => Promise<any>>;
}