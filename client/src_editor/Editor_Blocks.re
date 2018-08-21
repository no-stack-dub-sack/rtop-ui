/*
  TODO:
   - make wasDeletedWarningBlocks READONLY
   - do we want code blocks to be executed when blocks are deleted and / or restored?
   - confirm if we like way last block cases are handled
 */

[%%debugger.chrome];
Modules.require("./Editor_Blocks.css");

open Utils;
open Editor_Blocks_Utils;
open Editor_CodeBlockTypes;
open Editor_Types.Block;

type action =
  | Block_Add(id, blockTyp)
  | Block_Execute
  | Block_Queue_Delete(id)
  | Block_Delete_Queued(id)
  | Block_Delete_All_Queued
  | Block_Restore(id)
  | Block_Capture_Timeout_Id(id, Js.Global.timeoutId)
  | Block_Focus(id, blockTyp)
  | Block_Blur(id)
  | Block_UpdateValue(id, string, CodeMirror.EditorChange.t)
  | Block_AddWidgets(id, array(Widget.t))
  | Block_FocusUp(id)
  | Block_FocusDown(id);

type state = {
  blocks: array(block),
  deletedBlocks: array(block),
  deletedBlockMeta: array(blockTimeoutMeta),
  stateUpdateReason: option(action),
  focusedBlock: option((id, blockTyp, focusChangeType)),
};

let blockControlsButtons = (blockId, deletedBlocks, send) => {
  let isDeletedBlock =
    deletedBlocks
    ->(Belt.Array.keepU((. {b_id}) => b_id == blockId))
    ->Belt.Array.length
    > 0;
  <div className="block__controls--buttons">
    <UI_Balloon message="Add code block" position=Down>
      ...<button
           className="block__controls--button"
           onClick=(_ => send(Block_Add(blockId, BTyp_Code)))>
           <Fi.Code />
           <sup> "+"->str </sup>
         </button>
    </UI_Balloon>
    <UI_Balloon message="Add text block" position=Down>
      ...<button
           className="block__controls--button"
           onClick=(_ => send(Block_Add(blockId, BTyp_Text)))>
           <Fi.Edit2 />
           <sup> "+"->str </sup>
         </button>
    </UI_Balloon>
    (
      !isDeletedBlock ?
        <UI_Balloon message="Delete block" position=Down>
          ...<button
               className="block__controls--button block__controls--danger"
               onClick=(_ => send(Block_Queue_Delete(blockId)))>
               <Fi.Trash2 />
               <sup> "-"->str </sup>
             </button>
        </UI_Balloon> :
        <UI_Balloon message="Restore block" position=Down>
          ...<button
               className="block__controls--button"
               onClick=(_ => send(Block_Restore(blockId)))>
               <Fi.Refresh />
               <sup> "-"->str </sup>
             </button>
        </UI_Balloon>
    )
  </div>;
};

let component = ReasonReact.reducerComponent("Editor_Page");

let make =
    (
      ~blocks: array(block),
      ~readOnly=false,
      ~onUpdate,
      ~registerExecuteCallback=?,
      _children,
    ) => {
  ...component,
  initialState: () => {
    blocks: blocks->syncLineNumber,
    deletedBlocks: [||],
    deletedBlockMeta: [||],
    stateUpdateReason: None,
    focusedBlock: None,
  },
  didMount: self => {
    self.send(Block_Execute);
    switch (registerExecuteCallback) {
    | None => ()
    | Some(register) => register(() => self.send(Block_Execute))
    };
  },
  didUpdate: ({oldSelf, newSelf}) =>
    if (oldSelf.state.blocks !== newSelf.state.blocks) {
      switch (newSelf.state.stateUpdateReason) {
      | None => ()
      | Some(action) =>
        switch (action) {
        | Block_Focus(_, _)
        | Block_Blur(_)
        | Block_AddWidgets(_, _)
        | Block_FocusUp(_)
        | Block_FocusDown(_)
        | Block_Execute => ()
        | Block_Add(_, _)
        | Block_Queue_Delete(_)
        | Block_Delete_Queued(_)
        | Block_Delete_All_Queued
        | Block_Restore(_)
        | Block_Capture_Timeout_Id(_, _)
        | Block_UpdateValue(_, _, _) => onUpdate(newSelf.state.blocks)
        }
      };
    },
  reducer: (action, state) =>
    switch (action) {
    | Block_AddWidgets(blockId, widgets) =>
      ReasonReact.Update({
        ...state,
        stateUpdateReason: Some(action),
        blocks:
          state.blocks
          ->(
              Belt.Array.mapU((. block) => {
                let {b_id, b_data} = block;
                if (b_id != blockId) {
                  block;
                } else {
                  switch (b_data) {
                  | B_Text(_) => block
                  | B_Code(bcode) => {
                      b_id,
                      b_data: B_Code({...bcode, bc_widgets: widgets}),
                    }
                  };
                };
              })
            ),
      })
    | Block_Execute =>
      let allCodeBlocks =
        state.blocks
        ->(
            Belt.Array.reduceU([], (. acc, {b_id, b_data}) =>
              switch (b_data) {
              | B_Text(_) => acc
              | B_Code({bc_value}) => [(b_id, bc_value), ...acc]
              }
            )
          )
        ->Belt.List.reverse;

      /* Clear all widgets and execute all blocks */
      ReasonReact.SideEffects(
        (
          self =>
            Js.Promise.(
              Editor_Worker.executeMany(. allCodeBlocks)
              |> then_(results => {
                   results
                   ->(
                       Belt.List.forEachU((. (blockId, result)) => {
                         let widgets = executeResultToWidget(result);
                         self.send(Block_AddWidgets(blockId, widgets));
                       })
                     );

                   resolve();
                 })
              |> catch(error => resolve(Js.log(error)))
              |> ignore
            )
        ),
      );
    | Block_UpdateValue(blockId, newValue, diff) =>
      let blockIndex =
        arrayFindIndex(state.blocks, ({b_id}) => b_id == blockId)
        ->getBlockIndex;

      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          stateUpdateReason: Some(action),
          blocks:
            state.blocks
            ->(
                Belt.Array.mapWithIndexU((. i, block) => {
                  let {b_id, b_data} = block;
                  if (i < blockIndex) {
                    block;
                  } else if (i == blockIndex) {
                    switch (b_data) {
                    | B_Code(bcode) => {
                        b_id,
                        b_data:
                          B_Code({
                            ...bcode,
                            bc_value: newValue,
                            bc_widgets: {
                              let removeWidgetBelowMe =
                                diff->getFirstLineFromDiff;
                              let currentWidgets = bcode.bc_widgets;
                              currentWidgets
                              ->(
                                  Belt.Array.keepU(
                                    (.
                                      {
                                        Editor_CodeBlockTypes.Widget.lw_line,
                                        _,
                                      },
                                    ) =>
                                    lw_line < removeWidgetBelowMe
                                  )
                                );
                            },
                          }),
                      }
                    | B_Text(_) => {b_id, b_data: B_Text(newValue)}
                    };
                  } else {
                    switch (b_data) {
                    | B_Text(_) => block
                    | B_Code(bcode) => {
                        ...block,
                        b_data: B_Code({...bcode, bc_widgets: [||]}),
                      }
                    };
                  };
                })
              )
            ->syncLineNumber,
        },
        (self => self.send(Block_Delete_All_Queued)),
      );
    | Block_Queue_Delete(blockId) =>
      let lastBlock = Belt.Array.length(state.blocks) == 1;
      let warningBlock = {b_id: blockId, b_data: wasDeletedWarningBlock()};
      if (lastBlock) {
        let newBlock = {b_id: generateId(), b_data: emptyCodeBlock()};
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            deletedBlocks: [|state.blocks[0]|],
            blocks: [|newBlock, warningBlock|]->syncLineNumber,
            stateUpdateReason: Some(action),
            focusedBlock: None,
          },
          (
            self => {
              let timeoutId =
                Js.Global.setTimeout(
                  () => self.send(Block_Delete_Queued(blockId)),
                  10000,
                );
              self.send(Block_Capture_Timeout_Id(blockId, timeoutId));
            }
          ),
        );
      } else {
        let blockIndex =
          arrayFindIndex(state.blocks, ({b_id}) => b_id == blockId)
          ->getBlockIndex;
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            blocks:
              state.blocks
              ->(
                  Belt.Array.mapWithIndexU((. i, block) =>
                    i == blockIndex ? warningBlock : block
                  )
                )
              ->syncLineNumber,
            deletedBlocks:
              Belt.Array.concat(
                state.deletedBlocks,
                [|state.blocks[blockIndex]|],
              ),
            stateUpdateReason: Some(action),
            focusedBlock:
              switch (state.focusedBlock) {
              | None => None
              | Some((focusedBlock, _, _)) =>
                focusedBlock == blockId ? None : state.focusedBlock
              },
          },
          (
            self => {
              let timeoutId =
                Js.Global.setTimeout(
                  () => self.send(Block_Delete_Queued(blockId)),
                  10000,
                );
              self.send(Block_Capture_Timeout_Id(blockId, timeoutId));
            }
          ),
        );
      };
    | Block_Delete_Queued(blockId) =>
      let lastBlock = Belt.Array.length(state.blocks) == 1;
      if (lastBlock) {
        let newBlock = {b_id: generateId(), b_data: emptyCodeBlock()};
        ReasonReact.Update({
          blocks: [|newBlock|],
          deletedBlocks: [||],
          deletedBlockMeta:
            state.deletedBlockMeta
            ->(Belt.Array.keepU((. {id}) => id != blockId)),
          stateUpdateReason: Some(action),
          focusedBlock: None,
        });
      } else {
        ReasonReact.Update({
          blocks:
            state.blocks->(Belt.Array.keepU((. {b_id}) => b_id != blockId)),
          deletedBlocks:
            state.deletedBlocks
            ->(Belt.Array.keepU((. {b_id}) => b_id != blockId)),
          deletedBlockMeta:
            state.deletedBlockMeta
            ->(Belt.Array.keepU((. {id}) => id != blockId)),
          focusedBlock:
            switch (state.focusedBlock) {
            | None => None
            | Some((focusedBlock, _, _)) =>
              focusedBlock == blockId ? None : state.focusedBlock
            },
          stateUpdateReason: Some(action),
        });
      };
    | Block_Delete_All_Queued =>
      let b_ids = state.deletedBlockMeta->Belt.Array.mapU(((. {id}) => id));
      let t_ids =
        state.deletedBlockMeta->Belt.Array.mapU(((. {t_id}) => t_id));
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          blocks:
            state.blocks
            ->(
                Belt.Array.keepU((. {b_id}) =>
                  b_ids |> Js.Array.indexOf(b_id) == (-1)
                )
              ),
          deletedBlocks: [||],
          stateUpdateReason: Some(action),
        },
        (
          _self =>
            t_ids
            ->Belt.Array.forEachU((. t_id) => Js.Global.clearTimeout(t_id))
        ),
      );
    | Block_Restore(blockId) =>
      let blockIndex =
        arrayFindIndex(state.blocks, ({b_id}) => b_id == blockId)
        ->getBlockIndex;
      let restored = state.deletedBlocks
                     ->Belt.Array.keepU(((. {b_id}) => b_id == blockId))[0];
      let timeoutId =
        state.deletedBlockMeta
        ->Belt.Array.keepU(((. {id}) => id == blockId))[0].
          t_id;
      ReasonReact.UpdateWithSideEffects(
        {
          blocks:
            state.blocks
            ->(
                Belt.Array.mapWithIndexU((. i, block) =>
                  i == blockIndex ? restored : block
                )
              )
            ->syncLineNumber,
          deletedBlocks:
            state.deletedBlocks
            ->(Belt.Array.keepU((. {b_id}) => b_id != blockId)),
          deletedBlockMeta:
            state.deletedBlockMeta
            ->(Belt.Array.keepU((. {id}) => id != blockId)),
          stateUpdateReason: Some(action),
          focusedBlock:
            switch (state.focusedBlock) {
            | None => None
            | Some((focusedBlock, _, _)) =>
              focusedBlock == blockId ? None : state.focusedBlock
            },
        },
        (_self => Js.Global.clearTimeout(timeoutId)),
      );
    | Block_Capture_Timeout_Id(blockId, timeoutId) =>
      let blockTimeoutMeta = {id: blockId, t_id: timeoutId};
      ReasonReact.Update({
        ...state,
        deletedBlockMeta:
          Belt.Array.concat(state.deletedBlockMeta, [|blockTimeoutMeta|]),
        stateUpdateReason: Some(action),
      });
    | Block_Focus(blockId, blockTyp) =>
      ReasonReact.Update({
        ...state,
        stateUpdateReason: Some(action),
        focusedBlock: Some((blockId, blockTyp, FcTyp_EditorFocus)),
      })
    | Block_Blur(blockId) =>
      switch (state.focusedBlock) {
      | None => ReasonReact.NoUpdate
      | Some((focusedBlockId, _, _)) =>
        focusedBlockId == blockId ?
          ReasonReact.Update({
            ...state,
            stateUpdateReason: Some(action),
            focusedBlock: None,
          }) :
          ReasonReact.NoUpdate
      }
    | Block_Add(afterBlockId, blockTyp) =>
      let newBlockId = generateId();
      ReasonReact.Update({
        ...state,
        stateUpdateReason: Some(action),
        focusedBlock: Some((newBlockId, blockTyp, FcTyp_BlockNew)),
        blocks:
          state.blocks
          ->(
              Belt.Array.reduceU(
                [||],
                (. acc, block) => {
                  let {b_id} = block;
                  if (b_id != afterBlockId) {
                    Belt.Array.concat(acc, [|block|]);
                  } else {
                    Belt.Array.concat(
                      acc,
                      [|
                        block,
                        {
                          b_id: newBlockId,
                          b_data:
                            switch (blockTyp) {
                            | BTyp_Text => emptyTextBlock()
                            | BTyp_Code => emptyCodeBlock()
                            },
                        },
                      |],
                    );
                  };
                },
              )
            )
          ->syncLineNumber,
      });
    | Block_FocusUp(blockId) =>
      let upperBlock = {
        let rec loop = i =>
          if (i >= 0) {
            let {b_id} = state.blocks[i];
            if (b_id == blockId && i != 0) {
              let {b_id, b_data} = state.blocks[(i - 1)];
              switch (b_data) {
              | B_Code(_) => Some((b_id, BTyp_Code))
              | B_Text(_) => Some((b_id, BTyp_Text))
              };
            } else {
              loop(i - 1);
            };
          } else {
            None;
          };
        loop(state.blocks->Belt.Array.length - 1);
      };
      switch (upperBlock) {
      | None => ReasonReact.NoUpdate
      | Some((upperBlockId, blockTyp)) =>
        ReasonReact.Update({
          ...state,
          stateUpdateReason: Some(action),
          focusedBlock: Some((upperBlockId, blockTyp, FcTyp_BlockFocusUp)),
        })
      };
    | Block_FocusDown(blockId) =>
      let lowerBlock = {
        let length = state.blocks->Belt.Array.length;
        let rec loop = i =>
          if (i < length) {
            let {b_id} = state.blocks[i];
            if (b_id == blockId && i != length - 1) {
              let {b_id, b_data} = state.blocks[(i + 1)];
              switch (b_data) {
              | B_Code(_) => Some((b_id, BTyp_Code))
              | B_Text(_) => Some((b_id, BTyp_Text))
              };
            } else {
              loop(i + 1);
            };
          } else {
            None;
          };
        loop(0);
      };
      switch (lowerBlock) {
      | None => ReasonReact.NoUpdate
      | Some((lowerBlockId, blockTyp)) =>
        ReasonReact.Update({
          ...state,
          stateUpdateReason: Some(action),
          focusedBlock: Some((lowerBlockId, blockTyp, FcTyp_BlockFocusDown)),
        })
      };
    },
  render: ({send, state}) =>
    <>
      state.blocks
      ->(
          Belt.Array.mapU((. {b_id, b_data}) =>
            <div key=b_id id=b_id className="block__container">
              (
                switch (b_data) {
                | B_Code({bc_value, bc_widgets, bc_firstLineNumber}) =>
                  <div className="source-editor">
                    <Editor_CodeBlock
                      value=bc_value
                      focused=(
                        switch (state.focusedBlock) {
                        | None => None
                        | Some((id, _blockTyp, changeTyp)) =>
                          id == b_id ? Some(changeTyp) : None
                        }
                      )
                      onChange=(
                        (newValue, diff) => {
                          Js.log("onChange");
                          send(Block_UpdateValue(b_id, newValue, diff));
                        }
                      )
                      onExecute=(() => send(Block_Execute))
                      onFocus=(() => send(Block_Focus(b_id, BTyp_Code)))
                      onBlur=(() => send(Block_Blur(b_id)))
                      onBlockUp=(() => send(Block_FocusUp(b_id)))
                      onBlockDown=(() => send(Block_FocusDown(b_id)))
                      widgets=bc_widgets
                      readOnly
                      firstLineNumber=bc_firstLineNumber
                    />
                  </div>
                | B_Text(text) =>
                  <div className="text-editor">
                    <Editor_TextBlock
                      value=text
                      focused=(
                        switch (state.focusedBlock) {
                        | None => None
                        | Some((id, _blockTyp, changeTyp)) =>
                          id == b_id ? Some(changeTyp) : None
                        }
                      )
                      onFocus=(() => send(Block_Focus(b_id, BTyp_Text)))
                      onBlur=(() => send(Block_Blur(b_id)))
                      onBlockUp=(() => send(Block_FocusUp(b_id)))
                      onBlockDown=(() => send(Block_FocusDown(b_id)))
                      onChange=(
                        (newValue, diff) => {
                          Js.log("onChange");
                          send(Block_UpdateValue(b_id, newValue, diff));
                        }
                      )
                      readOnly
                    />
                  </div>
                }
              )
              (
                readOnly ?
                  React.null :
                  <div className="block__controls">
                    (blockControlsButtons(b_id, state.deletedBlocks, send))
                  </div>
              )
            </div>
          )
        )
      ->ReasonReact.array
    </>,
};
