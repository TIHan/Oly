import { defineConfig } from "eslint/config";
import js from "@eslint/js";
import tseslint from "typescript-eslint";
import globals from "globals";

export default defineConfig([
    // 1. Global ignores (replaces .eslintignore)
    {
        ignores: ["node_modules/"]
    },

    // 2. Base configuration for all JavaScript files
    js.configs.recommended,

    // 3. Configuration for TypeScript files
    ...tseslint.configs.recommended,

    // 4. Custom settings, globals, and rule overrides
    {
        files: ["**/*.{js,mjs,cjs,ts,tsx}"],
        languageOptions: {
            ecmaVersion: "latest",
            sourceType: "module",
            globals: {
                ...globals.browser,
                ...globals.node,
                myCustomGlobal: "readonly"
            }
        },
        rules: {
            "no-unused-vars": [
                "error",
                { argsIgnorePattern: "^_", varsIgnorePattern: "^_" },
            ],
            "no-console": ["warn", { allow: ["warn", "error"] }],
            "prefer-const": "error",
            "@typescript-eslint/no-explicit-any": "off",
            "@typescript-eslint/no-unused-vars": [
                "error",
                { argsIgnorePattern: "^_", varsIgnorePattern: "^_" },
            ]
        }
    }
]);