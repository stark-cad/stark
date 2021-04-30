// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is free software: you can redistribute it and / or modify it
// under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.

// STARK is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public
// License along with STARK (in the LICENSE file). If not, see
// <https://www.gnu.org/licenses/>.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/lib.rs

// Top level lib: declares features and modules, and contains some
// utility functions

// <>

#![feature(core_intrinsics)]
#![feature(const_fn_transmute)]

use raw_window_handle::{RawWindowHandle, HasRawWindowHandle};

pub mod context;
pub mod graphics;
pub mod sail;

pub struct WinHandle(pub RawWindowHandle);

unsafe impl HasRawWindowHandle for WinHandle {
    fn raw_window_handle(&self) -> RawWindowHandle {
        self.0
    }
}

unsafe impl Send for WinHandle {}
