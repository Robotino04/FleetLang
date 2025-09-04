use std::{
    io::Error,
    pin::Pin,
    task::{Context, Poll},
};

use futures::AsyncWrite;

enum State {
    Idle,
    Flushing(usize),
}

pub trait ToFlushOnWrite
where
    Self: AsyncWrite + Sized,
{
    fn flush_on_write(self) -> FlushOnWrite<Self>;
}

impl<W> ToFlushOnWrite for W
where
    W: AsyncWrite,
{
    fn flush_on_write(self) -> FlushOnWrite<Self> {
        FlushOnWrite::new(self)
    }
}

pub struct FlushOnWrite<W>
where
    W: AsyncWrite,
{
    writer: W,
    state: State,
}

impl<W> FlushOnWrite<W>
where
    W: AsyncWrite,
{
    fn new(writer: W) -> Self {
        Self {
            writer,
            state: State::Idle,
        }
    }
}

impl<W> AsyncWrite for FlushOnWrite<W>
where
    W: AsyncWrite + Unpin,
{
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, Error>> {
        let this = &mut *self;

        loop {
            match this.state {
                State::Idle => {
                    match Pin::new(&mut this.writer).poll_write(cx, buf) {
                        Poll::Ready(Ok(0)) => return Poll::Ready(Ok(0)),
                        Poll::Ready(Ok(n)) => {
                            this.state = State::Flushing(n);
                            continue;
                        }
                        other => return other,
                    };
                }
                State::Flushing(n) => match Pin::new(&mut this.writer).poll_flush(cx) {
                    Poll::Ready(Ok(())) => {
                        this.state = State::Idle;
                        return Poll::Ready(Ok(n));
                    }
                    Poll::Ready(Err(e)) => {
                        this.state = State::Idle;
                        return Poll::Ready(Err(e));
                    }
                    Poll::Pending => return Poll::Pending,
                },
            }
        }
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Error>> {
        Pin::new(&mut self.writer).poll_flush(cx)
    }

    fn poll_close(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), Error>> {
        Pin::new(&mut self.writer).poll_close(cx)
    }
}
