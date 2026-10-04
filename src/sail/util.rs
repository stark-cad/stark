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
struct LockToken(*mut u8);

// TODO: support more than one lock? (symtab)
trait Lock
where
    Self: Sized,
{
    fn lock_pos(self: Hdl<Self>) -> *mut u8;

    #[must_use]
    fn take_lock(self: Hdl<Self>) -> LockToken {
        let lock = self.lock_pos();
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
        LockToken(lock)
    }

    #[must_use]
    fn take_lock_nb(self: Hdl<Self>) -> Option<LockToken> {
        let lock = self.lock_pos();
        unsafe {
            if std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                lock,
                false as u8,
                true as u8,
            )
            .1
            {
                Some(LockToken(lock))
            } else {
                None
            }
        }
    }

    fn release_lock(self: Hdl<Self>, token: LockToken) {
        assert_eq!(token.0, self.lock_pos());
        let lock = token.0;
        unsafe {
            std::intrinsics::atomic_store::<_, { AO::Release }, false>(lock, false as u8);
        }
    }

    fn locked_p(self: Hdl<Self>) -> bool {
        let lock = self.lock_pos();
        unsafe { std::intrinsics::atomic_load::<_, { AO::Acquire }, false>(lock) != 0 }
    }
}
