// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2026 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/util.rs

// General-purpose Sail utilities. Object handling. Components for
// runtime systems. Types to improve ergonomics.

// <>

// TODO: generic 'safe handle' type and lock trait?

use std::ops;
use std::ptr;

use std::intrinsics::AtomicOrdering as AO;

// TODO: find a better / clearer name for this than Hdl

/// Handle for persistent and unmoving runtime structures
#[repr(transparent)]
pub struct Hdl<T> {
    inner: ptr::NonNull<T>,
}

impl<T> ops::Receiver for Hdl<T> {
    type Target = T;
}

unsafe impl<T> Send for Hdl<T> {}

impl<T> Clone for Hdl<T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner }
    }
}

impl<T> Copy for Hdl<T> {}

impl<T> Hdl<T> {
    pub fn new(raw: *mut T) -> Self {
        Self {
            inner: ptr::NonNull::new(raw).unwrap(),
        }
    }

    pub fn raw(self) -> *mut T {
        self.inner.as_ptr()
    }
}

// TODO: autorelease or panic if dropped?
pub struct LockToken<T>(*mut u8, std::marker::PhantomData<T>);

#[inline(always)]
pub(crate) fn take_lock_at(lock: *mut u8) -> LockToken<()> {
    unsafe {
        while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
            lock,
            false as u8,
            true as u8,
        )
        .1
        {
            std::hint::spin_loop();
        }
    }
    LockToken(lock, std::marker::PhantomData)
}

#[inline(always)]
pub(crate) fn release_lock_at(token: LockToken<()>) {
    let lock = token.0;
    unsafe {
        std::intrinsics::atomic_store::<_, { AO::Release }, false>(lock, false as u8);
    }
}

#[inline(always)]
pub(crate) fn locked_p_at(lock: *const u8) -> bool {
    unsafe { std::intrinsics::atomic_load::<_, { AO::Acquire }, false>(lock) != false as u8 }
}

pub trait Lock
where
    Self: Sized,
{
    fn lock_pos(self: Hdl<Self>) -> *mut u8;

    #[must_use]
    fn take_lock(self: Hdl<Self>) -> LockToken<Self> {
        let lock = self.lock_pos();
        LockToken(take_lock_at(lock).0, std::marker::PhantomData)
    }

    #[must_use]
    fn take_lock_nb(self: Hdl<Self>) -> Option<LockToken<Self>> {
        let lock = self.lock_pos();
        unsafe {
            if std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                lock,
                false as u8,
                true as u8,
            )
            .1
            {
                Some(LockToken(lock, std::marker::PhantomData))
            } else {
                None
            }
        }
    }

    fn release_lock(self: Hdl<Self>, token: LockToken<Self>) {
        assert_eq!(token.0, self.lock_pos());
        release_lock_at(LockToken(token.0, std::marker::PhantomData))
    }

    fn locked_p(self: Hdl<Self>) -> bool {
        let lock = self.lock_pos();
        locked_p_at(lock)
    }
}

    }
}
