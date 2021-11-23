! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Tools to create pretty diagnostic reports
module diagnostic
   use diagnostic_color, only : color_type
   use diagnostic_type, only : diagnostic_report, label_type, render, &
      level_error, level_warning, level_help, level_note, level_info
   implicit none
   public

end module diagnostic
