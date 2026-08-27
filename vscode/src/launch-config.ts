export type LaunchConfiguration = Record<string, unknown>;

export function parseLaunchConfigurations(
  value: unknown
): LaunchConfiguration[] {
  if (value === undefined) {
    return [];
  }
  if (
    !Array.isArray(value) ||
    value.some(
      (entry) =>
        typeof entry !== "object" || entry === null || Array.isArray(entry)
    )
  ) {
    throw new Error("launch.json 'configurations' must be an array of objects.");
  }
  return value as LaunchConfiguration[];
}

export function hasLaunchConfiguration(
  configurations: readonly LaunchConfiguration[],
  name: string
): boolean {
  return configurations.some((configuration) => configuration.name === name);
}

export function writableLaunchConfigurations(
  workspaceValue: unknown,
  workspaceFolderValue: unknown,
  hasWorkspaceFile: boolean
): LaunchConfiguration[] {
  return parseLaunchConfigurations(
    hasWorkspaceFile ? workspaceFolderValue : workspaceValue
  );
}

export function createMosLaunchConfiguration(
  name: string,
  vicePath: string
): LaunchConfiguration {
  return {
    type: "mos",
    request: "launch",
    name,
    workspace: "${workspaceFolder}",
    preLaunchTask: "mos: Build",
    vicePath
  };
}
