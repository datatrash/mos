use lsp_server::Message;
use lsp_types::notification::Notification;

#[derive(Debug, PartialEq)]
pub struct MosNotification<A: Notification> {
    pub params: A::Params,
}

impl<A: Notification> MosNotification<A> {
    pub fn new(params: A::Params) -> MosNotification<A> {
        MosNotification { params }
    }
}

impl<A: Notification> Into<Message> for MosNotification<A> {
    fn into(self) -> Message {
        let params = serde_json::to_value(&self.params).unwrap();
        let n = lsp_server::Notification {
            method: A::METHOD.into(),
            params,
        };
        Message::Notification(n)
    }
}
