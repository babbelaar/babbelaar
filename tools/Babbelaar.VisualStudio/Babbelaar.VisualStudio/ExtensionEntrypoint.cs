using Microsoft.Extensions.DependencyInjection;
using Microsoft.VisualStudio.Extensibility;
using Microsoft.VisualStudio.Extensibility.Editor;

namespace Babbelaar.VisualStudio;

/// <summary>
/// Extension entrypoint for the VisualStudio.Extensibility extension.
/// </summary>
[VisualStudioContribution]
internal class ExtensionEntrypoint : Extension
{
    /// <inheritdoc/>
    public override ExtensionConfiguration ExtensionConfiguration => new()
    {
        Metadata = new(
            id: "Babbelaar.VisualStudio.c0ef11a6-c3be-42ef-b317-1d91ed9e3043",
            version: this.ExtensionAssemblyVersion,
            publisherName: "Babbelaar",
            displayName: "Babbelaar voor Visual Studio",
            description: "Ondersteuning voor de Babbelaar-taal in Visual Studio"),
    };

    /// <inheritdoc />
    protected override void InitializeServices(IServiceCollection serviceCollection)
    {
        base.InitializeServices(serviceCollection);

        // You can configure dependency injection here by adding services to the serviceCollection.
    }
}
