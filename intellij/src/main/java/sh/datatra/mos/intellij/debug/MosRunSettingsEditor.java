package sh.datatra.mos.intellij.debug;

import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComboBox;
import com.intellij.openapi.ui.TextBrowseFolderListener;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import com.redhat.devtools.lsp4ij.settings.ServerTrace;
import org.jetbrains.annotations.NotNull;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComponent;
import javax.swing.JPanel;

final class MosRunSettingsEditor extends SettingsEditor<MosRunConfiguration> {
    private final JPanel panel;
    private final TextFieldWithBrowseButton vicePath = new TextFieldWithBrowseButton();
    private final JBTextField testCaseName = new JBTextField();
    private final ComboBox<ServerTrace> serverTrace =
            new ComboBox<>(new DefaultComboBoxModel<>(ServerTrace.values()));

    MosRunSettingsEditor(Project project) {
        vicePath.addBrowseFolderListener(new TextBrowseFolderListener(
                FileChooserDescriptorFactory.createSingleFileOrExecutableAppDescriptor()
                        .withTitle("VICE executable")
                        .withDescription(
                                "Select the VICE 3.5 or newer executable. "
                                        + "Leave empty for test configurations."
                        ),
                project
        ));
        panel = FormBuilder.createFormBuilder()
                .addLabeledComponent("VICE executable:", vicePath)
                .addLabeledComponent("Test case:", testCaseName)
                .addLabeledComponent("Protocol trace:", serverTrace)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
    }

    @Override
    protected void resetEditorFrom(@NotNull MosRunConfiguration configuration) {
        MosRunConfigurationOptions options = configuration.getOptions();
        vicePath.setText(options.getVicePath());
        testCaseName.setText(options.getTestCaseName());
        serverTrace.setSelectedItem(options.getServerTrace());
    }

    @Override
    protected void applyEditorTo(@NotNull MosRunConfiguration configuration)
            throws ConfigurationException {
        MosRunConfigurationOptions options = configuration.getOptions();
        options.setVicePath(vicePath.getText().trim());
        options.setTestCaseName(testCaseName.getText().trim());
        options.setServerTrace((ServerTrace) serverTrace.getSelectedItem());
    }

    @Override
    protected @NotNull JComponent createEditor() {
        return panel;
    }
}
