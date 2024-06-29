using Microsoft.VisualStudio.Extensibility;
using Microsoft.VisualStudio.Extensibility.Editor;
using Microsoft.VisualStudio.Extensibility.LanguageServer;
using Microsoft.VisualStudio.Extensibility.Shell;
using Nerdbank.Streams;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.IO.Pipelines;
using System.Reflection;

namespace Babbelaar.VisualStudio;

[VisualStudioContribution]
[Experimental("VSEXTPREVIEW_LSP")]
public sealed class BabbelaarLanguageServerProvider : LanguageServerProvider
{
    [VisualStudioContribution]
    public static DocumentTypeConfiguration BabbelaarDocumentType => new("babbelaar")
    {
        FileExtensions = [".bab"],
        BaseDocumentType = LanguageServerBaseDocumentType,
    };

    public override LanguageServerProviderConfiguration LanguageServerProviderConfiguration => new(
        "%Babbelaar%",
        [DocumentFilter.FromDocumentType(BabbelaarDocumentType)]);


    public override async Task<IDuplexPipe?> CreateServerConnectionAsync(CancellationToken cancellationToken)
    {
        while (true)
        {
            try
            {
                return DoCreateServerConnection();
            }
            catch (Exception ex)
            {
                bool shouldRetry = await Extensibility.Shell().ShowPromptAsync(ex.ToString(), PromptOptions.RetryCancel, cancellationToken);

                if (!shouldRetry)
                {
                    return null;
                }
            }
        }
    }

    private static DuplexPipe? DoCreateServerConnection()
    {
        ProcessStartInfo info = new()
        {
            FileName = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!, @"babbelaar-lsp.exe"),
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            UseShellExecute = false,
            CreateNoWindow = true
        };

        Process process = new()
        {
            StartInfo = info
        };

        if (process.Start())
        {
            return new DuplexPipe(
                PipeReader.Create(process.StandardOutput.BaseStream),
                PipeWriter.Create(process.StandardInput.BaseStream)
            );
        }

        return null;
    }
}
