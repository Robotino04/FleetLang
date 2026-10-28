use std::{
    any::{Any, TypeId, type_name},
    boxed::Box,
    collections::{HashMap, hash_map::Entry},
    fmt::Debug,
    marker::PhantomData,
    option::Option,
};

use thiserror::Error;

#[derive(Copy, Clone, Debug, Error)]
pub enum FetchError {
    #[error("Failed to fetch shared reference to `{1}` because it wasn't present")]
    NotPresent(TypeId, &'static str),
    #[error("Failed to fetch mutable reference to `{1}` because it wasn't present")]
    NotPresentMut(TypeId, &'static str),
    #[error("Failed to fetch `{1}` because it wasn't empty")]
    NotEmpty(TypeId, &'static str),
}

type Slot = Option<Box<dyn Any>>;

#[derive(Debug, Default)]
pub struct GlobalState {
    entries: HashMap<TypeId, Slot>,
}

impl GlobalState {
    pub fn fetch<'a, R, W, E>(&'a mut self) -> Result<Fetched<R, W, E>, FetchError>
    where
        Fetched<R, W, E>: Fetchable<'a> + 'a,
    {
        Fetched::fetch(&mut self.entries)
    }

    #[must_use]
    pub fn get<R: 'static>(&self) -> Option<&R> {
        Some(
            self.entries
                .get(&TypeId::of::<R>())
                .and_then(Option::as_ref)?
                .downcast_ref()
                .unwrap(),
        )
    }

    #[must_use]
    pub fn get_mut<W: 'static>(&mut self) -> Option<&mut W> {
        Some(
            self.entries
                .get_mut(&TypeId::of::<W>())
                .and_then(Option::as_mut)?
                .downcast_mut()
                .unwrap(),
        )
    }

    #[must_use]
    pub fn get_empty<'a, W: 'static>(&'a mut self) -> Option<Empty<'a, W>> {
        match self.entries.entry(TypeId::of::<W>()) {
            Entry::Occupied(mut entry) => match entry.get_mut() {
                Some(_) => None,
                None => Some(Empty(entry.into_mut(), PhantomData)),
            },
            Entry::Vacant(entry) => Some(Empty(entry.insert(None), PhantomData)),
        }
    }

    #[must_use]
    pub fn insert<T: 'static>(&mut self, t: T) -> Option<&mut T> {
        Some(self.get_empty()?.insert(t))
    }

    #[must_use]
    pub fn insert_default<T: Default + 'static>(&mut self) -> Option<&mut T> {
        Some(self.get_empty()?.insert_default::<T>())
    }
}

#[derive(Debug)]
pub struct Empty<'a, T: 'a>(&'a mut Option<Box<dyn Any>>, PhantomData<T>);

trait Same {
    type Type: ?Sized;
}
impl<T: ?Sized> Same for T {
    type Type = T;
}

impl<'a, T: 'static> Empty<'a, T> {
    #[expect(private_bounds)]
    pub fn insert<U>(self, t: U) -> &'a mut T
    where
        T: Same<Type = U>,
        U: Same<Type = T> + 'static,
    {
        assert!(
            self.0.is_none(),
            "Tried inserting value into nonempty slot {} via Empty",
            type_name::<T>()
        );
        self.0.insert(Box::new(t)).downcast_mut().unwrap()
    }

    #[expect(private_bounds)]
    pub fn insert_default<U>(self) -> &'a mut T
    where
        T: Same<Type = U>,
        U: Same<Type = T> + Default + 'static,
    {
        self.insert(Default::default())
    }
}

pub trait Fetchable<'a> {
    fn fetch(entries: &'a mut HashMap<TypeId, Slot>) -> Result<Self, FetchError>
    where
        Self: Sized + 'a;
}

pub struct Fetched<R, W, E> {
    pub read: R,
    pub write: W,
    pub empty: E,
}

macro_rules! impl_fetchable {
    (
        $($R:ident, )*;
        $($W:ident, )*;
        $($E:ident, )*
    ) => {
        impl<
            'a,
            $($R: 'static, )*
            $($W: 'static, )*
            $($E: 'static, )*
        > Fetchable<'a>
            for Fetched<
                ($(&'a $R, )*),
                ($(&'a mut $W, )*),
                ($(Empty<'a, $E>, )*),
            >
        {
            fn fetch(entries: &'a mut HashMap<TypeId, Slot>) -> Result<Self, FetchError>
            where
                Self: Sized + 'a,
            {
                $(
                    match entries.entry(TypeId::of::<$E>()) {
                        Entry::Occupied(mut entry) => match entry.get_mut() {
                            Some(_) => return Err(FetchError::NotEmpty(TypeId::of::<$E>(), type_name::<$E>())),
                            None => {},
                        },
                        Entry::Vacant(entry) => {
                            entry.insert(None);
                        }
                    }
                )*

                #[allow(non_snake_case)]
                let [
                    $($R, )*
                    $($W, )*
                    $($E, )*
                ] = entries.get_disjoint_mut([
                    $(&TypeId::of::<$R>(), )*
                    $(&TypeId::of::<$W>(), )*
                    $(&TypeId::of::<$E>(), )*
                ]);

                Ok(Self {
                    read: ($(
                        $R
                            .and_then(Option::as_mut)
                            .ok_or_else(|| FetchError::NotPresent(TypeId::of::<$R>(), type_name::<$R>()))?
                            .downcast_ref()
                            .unwrap(),
                    )*),
                    write: ($(
                        $W
                            .and_then(Option::as_mut)
                            .ok_or_else(|| FetchError::NotPresentMut(TypeId::of::<$W>(), type_name::<$W>()))?
                            .downcast_mut()
                            .unwrap(),
                    )*),
                    empty: ( $(Empty($E.unwrap(), PhantomData), )* ),
                })
            }
        }
    };
}

/// Generates one implementation for all trisections of the given tuple.
macro_rules! impl_fetchable_all {
    (
        $($A0:ident $(, $A:ident)*)? $(,)?
    ) => {
        impl_fetchable_all!(
            @first_split
            $($A0, $($A, )*)?
            ;
        );
    };
    (
        @first_split
        ;
        $($B:ident, )*
    ) => {
        impl_fetchable_all!(
            @second_split
            ;
            $($B, )*
            ;
        );
    };
    (
        @first_split
        $A0:ident, $($A:ident, )*
        ;
        $($B:ident, )*
    ) => {
        impl_fetchable_all!(
            @second_split
            $A0, $($A, )*
            ;
            $($B, )*
            ;
        );
        impl_fetchable_all!(
            @first_split
            $($A,)*
            ;
            $($B, )* $A0,
        );
    };

    (
        @second_split
        $($A:ident, )*
        ;
        ;
        $($C:ident, )*
    ) => {
        impl_fetchable_all!(
            @done
            $($A, )*
            ;
            ;
            $($C, )*
        );
    };
    (
        @second_split
        $($A:ident, )*
        ;
        $B0:ident, $($B:ident, )*
        ;
        $($C:ident, )*
    ) => {
        impl_fetchable_all!(
            @done
            $($A, )*
            ;
            $B0, $($B, )*
            ;
            $($C, )*
        );
        impl_fetchable_all!(
            @second_split
            $($A, )*
            ;
            $($B, )*
            ;
            $($C, )* $B0,
        );
    };
    (
        @done
        $($A:ident, )*
        ;
        $($B:ident, )*
        ;
        $($C:ident, )*
    ) => {
        impl_fetchable!($($C, )*; $($B, )*; $($A, )*);
    }
}

impl_fetchable_all!();
impl_fetchable_all!(A);
impl_fetchable_all!(A, B);
impl_fetchable_all!(A, B, C);
impl_fetchable_all!(A, B, C, D);
impl_fetchable_all!(A, B, C, D, E);
impl_fetchable_all!(A, B, C, D, E, F);
impl_fetchable_all!(A, B, C, D, E, F, G);
impl_fetchable_all!(A, B, C, D, E, F, G, H);
impl_fetchable_all!(A, B, C, D, E, F, G, H, I);
impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J);
impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J, K);
// impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J, K, L);
// impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J, K, L, M);
// impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
// impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
// impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);
// impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q);
// impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R);
// impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S);
// impl_fetchable_all!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T);
