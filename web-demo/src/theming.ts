import * as monaco from "monaco-editor";

export function convertVsCodeThemeToStandalone(
  themeJson: any,
  baseTheme: monaco.editor.BuiltinTheme = "vs-dark",
  inherit: boolean = true,
): monaco.editor.IStandaloneThemeData {
  // Extract UI colors
  // VS Code themes use "colors" property for UI tokens
  const colors = themeJson.colors || {};

  // Extract token colors rules
  // VS Code theme 'tokenColors' is an array with scopes and settings
  const tokenColors = themeJson.tokenColors || [];

  // Map to ITokenThemeRule format
  const rules = tokenColors.flatMap((rule: any) => {
    // VS Code tokenColors can have multiple scopes (string or array)
    const scopes = Array.isArray(rule.scope) ? rule.scope : [rule.scope];

    // Some rules may not have scopes or settings — skip those
    if (!scopes || !rule.settings) return [];

    return scopes.map((scope: any) => {
      const convertedRule: monaco.editor.ITokenThemeRule = {
        token: scope,
      };

      if (rule.settings.foreground) {
        // Remove leading '#' for the color to match typical token format
        convertedRule.foreground = rule.settings.foreground.replace(/^#/, "");
      }
      if (rule.settings.background) {
        convertedRule.background = rule.settings.background.replace(/^#/, "");
      }
      if (rule.settings.fontStyle) {
        convertedRule.fontStyle = rule.settings.fontStyle;
      }

      return convertedRule;
    });
  });

  return {
    base: baseTheme,
    inherit,
    rules,
    colors,
  };
}
