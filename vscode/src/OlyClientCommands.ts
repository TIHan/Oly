export namespace OlyClientCommands {
	// Client to Server commands
	export const navigateToSyntaxNode = `oly.navigateToSyntaxNode`;
	export const getSyntaxTree = `oly.getSyntaxTree`;
	export const compile = `oly.compile`;
	export const changeActiveProject = `oly.changeActiveProject`;
	export const changeActiveConfiguration = `oly.changeActiveConfiguration`;
	export const cleanWorkspace = `oly.cleanWorkspace`;

	// Client commands
	export const debug = `workbench.action.debug.start`;
	export const run = `workbench.action.debug.run`;
	export const createFile = 'oly.createFile';
	export const deleteFile = `oly.deleteFile`;
}
