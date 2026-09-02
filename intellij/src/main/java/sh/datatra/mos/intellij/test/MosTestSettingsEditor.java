package sh.datatra.mos.intellij.test;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import org.jetbrains.annotations.NotNull;

import javax.swing.JComponent;
import javax.swing.JPanel;

final class MosTestSettingsEditor extends SettingsEditor<MosTestRunConfiguration> {
    private final JPanel panel;
    private final JBTextField filter = new JBTextField();

    MosTestSettingsEditor(Project project) {
        panel = FormBuilder.createFormBuilder()
                .addLabeledComponent(new JBLabel("Test name filter:"), filter)
                .addComponent(
                        new JBLabel("Leave empty to run every test in the project. "
                                + "A filter runs only tests whose name contains it.")
                )
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
    }

    @Override
    protected void resetEditorFrom(@NotNull MosTestRunConfiguration configuration) {
        filter.setText(configuration.getOptions().getFilter());
    }

    @Override
    protected void applyEditorTo(@NotNull MosTestRunConfiguration configuration)
            throws ConfigurationException {
        configuration.getOptions().setFilter(filter.getText().trim());
    }

    @Override
    protected @NotNull JComponent createEditor() {
        return panel;
    }
}