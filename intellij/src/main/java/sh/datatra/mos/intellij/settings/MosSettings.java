package sh.datatra.mos.intellij.settings;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;

@Service(Service.Level.APP)
@State(name = "MosSettings", storages = @Storage("mos.xml"))
public final class MosSettings implements PersistentStateComponent<MosSettings.State> {
    private final State state = new State();

    public static MosSettings getInstance() {
        return com.intellij.openapi.application.ApplicationManager.getApplication().getService(MosSettings.class);
    }

    @Override
    public @NotNull State getState() {
        return state;
    }

    @Override
    public void loadState(@NotNull State loaded) {
        XmlSerializerUtil.copyBean(loaded, state);
    }

    public static final class State {
        public String executablePath = "";
        public boolean checkForUpdates = true;
        public boolean applicationCodeLens = true;
        public String vicePath = "";
        public String managedExecutable = "";
        public String managedTag = "";
        public String managedTarget = "";
    }
}
