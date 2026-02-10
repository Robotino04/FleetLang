/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2024 TypeFox and others.
 * Licensed under the MIT License. See LICENSE in the package root for license information.
 * ------------------------------------------------------------------------------------------ */

export const disableElement = (id: string, disabled: boolean) => {
    const button = document.getElementById(id) as HTMLButtonElement | HTMLInputElement | null;
    if (button !== null) {
        button.disabled = disabled;
    }
};

export const createDefaultWorkspaceContent = (workspacePath: string) => {
    return JSON.stringify(
        {
            folders: [
                {
                    path: workspacePath
                }
            ]
        },
        null,
        2
    );
};
