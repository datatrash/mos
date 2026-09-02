package sh.datatra.mos.intellij.lang;

import com.intellij.execution.lineMarker.RunLineMarkerContributor;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import sh.datatra.mos.intellij.actions.MosRunConfigurations;

/**
 * Renders a green run icon in the gutter for each {@code .test "name" { ... }} block, with a popup
 * offering Run and Debug, matching how IntelliJ surfaces run/debug for other languages instead of
 * relying on CodeLens.
 */
public final class MosTestLineMarkerContributor extends RunLineMarkerContributor {
    @Override
    public @Nullable Info getInfo(@NotNull PsiElement element) {
        var node = element.getNode();
        if (node == null || node.getElementType() != MosTokenTypes.TEST_DEFINITION) {
            return null;
        }
        String testName = TestNameLocator.find(element);
        if (testName == null) {
            return null;
        }
        return new Info(
                AllIcons.RunConfigurations.TestState.Run,
                new AnAction[]{
                        new MosGutterAction("Run " + testName, AllIcons.Actions.RunAll, testName, false),
                        new MosGutterAction("Debug " + testName, AllIcons.Actions.StartDebugger, testName, true)
                },
                element1 -> "Run/Debug " + testName
        );
    }

    private static final class MosGutterAction extends AnAction implements DumbAware {
        private final String testName;
        private final boolean debug;

        MosGutterAction(@NotNull String text, @Nullable javax.swing.Icon icon, @NotNull String testName, boolean debug) {
            super(text, null, icon);
            this.testName = testName;
            this.debug = debug;
        }

        @Override
        public void actionPerformed(@NotNull AnActionEvent event) {
            if (debug) {
                MosRunConfigurations.execute(event.getProject(), testName, true);
            } else {
                MosRunConfigurations.runTests(event.getProject(), testName);
            }
        }
    }
}