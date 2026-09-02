package sh.datatra.mos.intellij.settings;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.ui.TextBrowseFolderListener;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.util.ui.FormBuilder;
import com.redhat.devtools.lsp4ij.LanguageServerManager;
import com.redhat.devtools.lsp4ij.server.LanguageServerException;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;
import sh.datatra.mos.intellij.lsp.MosProjectRuntime;

import javax.swing.JComponent;
import javax.swing.JPanel;
import java.util.Objects;

public final class MosSettingsConfigurable implements Configurable {
    private static final Logger LOG = Logger.getInstance(MosSettingsConfigurable.class);

    private TextFieldWithBrowseButton executablePath;
    private TextFieldWithBrowseButton vicePath;
    private JBCheckBox checkForUpdates;
    private JBCheckBox applicationCodeLens;
    private JPanel panel;

    @Override
    public @Nls String getDisplayName() {
        return "MOS";
    }

    @Override
    public @Nullable JComponent createComponent() {
        executablePath = new TextFieldWithBrowseButton();
        executablePath.addBrowseFolderListener(new TextBrowseFolderListener(
                FileChooserDescriptorFactory.createSingleFileOrExecutableAppDescriptor()
                        .withTitle("MOS executable")
                        .withDescription(
                                "Select a custom MOS executable. Leave empty to use the managed installation."
                        )
        ));
        vicePath = new TextFieldWithBrowseButton();
        vicePath.addBrowseFolderListener(new TextBrowseFolderListener(
                FileChooserDescriptorFactory.createSingleFileOrExecutableAppDescriptor()
                        .withTitle("VICE executable")
                        .withDescription(
                                "Select the VICE 3.5 or newer executable used to run MOS applications."
                        )
        ));
        checkForUpdates = new JBCheckBox("Check for MOS updates when the language server starts");
        applicationCodeLens = new JBCheckBox("Show Run MOS and Debug MOS gutter actions");
        panel = FormBuilder.createFormBuilder()
                .addLabeledComponent("MOS executable:", executablePath)
                .addComponentToRightColumn(checkForUpdates)
                .addComponentToRightColumn(applicationCodeLens)
                .addLabeledComponent("VICE executable:", vicePath)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
        reset();
        return panel;
    }

    @Override
    public boolean isModified() {
        MosSettings.State state = MosSettings.getInstance().getState();
        return !Objects.equals(state.executablePath, executablePath.getText().trim())
                || !Objects.equals(state.vicePath, vicePath.getText().trim())
                || state.checkForUpdates != checkForUpdates.isSelected()
                || state.applicationCodeLens != applicationCodeLens.isSelected();
    }

    @Override
    public void apply() {
        MosSettings.State state = MosSettings.getInstance().getState();
        boolean restart = !Objects.equals(state.executablePath, executablePath.getText().trim());
        state.executablePath = executablePath.getText().trim();
        state.vicePath = vicePath.getText().trim();
        state.checkForUpdates = checkForUpdates.isSelected();
        state.applicationCodeLens = applicationCodeLens.isSelected();
        if (restart) {
            restartLanguageServers();
        }
    }

    @Override
    public void reset() {
        if (executablePath == null) {
            return;
        }
        MosSettings.State state = MosSettings.getInstance().getState();
        executablePath.setText(state.executablePath);
        vicePath.setText(state.vicePath);
        checkForUpdates.setSelected(state.checkForUpdates);
        applicationCodeLens.setSelected(state.applicationCodeLens);
    }

    @Override
    public void disposeUIResources() {
        executablePath = null;
        vicePath = null;
        checkForUpdates = null;
        applicationCodeLens = null;
        panel = null;
    }

    private static void restartLanguageServers() {
        LanguageServerManager.StartOptions options = new LanguageServerManager.StartOptions()
                .setForceStart(false)
                .setForceRestart(true);
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            try {
                LanguageServerManager.getInstance(project).start(MosProjectRuntime.SERVER_ID, options);
            } catch (LanguageServerException error) {
                LOG.warn("Could not restart the MOS language server for " + project.getName(), error);
            }
        }
    }
}
